#!/usr/bin/env Rscript
library(drake)
library(fortebaseline)
library(ggplot2)

# begin imports
import::from("dplyr", "tbl", "filter", "select", "collect", "mutate",
             "pull", "case_when", "rename", "ungroup", "group_by", "left_join",
             "if_else", "group_by_at", "bind_rows", .into = "")
import::from("tidyr", "unnest", .into = "")
import::from("tibble", "as_tibble", .into = "")
import::from("here", "here", .into = "")
import::from("fs", "dir_create", "path", .into = "")
import::from("fst", "write_fst", .into = "")
import::from("forcats", "fct_relabel", .into = "")
import::from("cowplot", "save_plot", "theme_cowplot", .into = "")
import::from("furrr", "future_pmap_dfr", .into = "")
import::from("magrittr", "%>%", .into = "")
import::from("purrr", "map", "possibly", .into = "")
import::from("future.callr", "callr", .into = "")
import::from("future", "plan", "availableCores", .into = "")
# end imports

expose_imports("fortebaseline")

created_since <- "2019-03-20"

workflow_df <- tbl(default_connection(), "workflows") %>%
  filter(start_date == "1902-06-01",
         end_date == "1912-12-31",
         created_at > created_since) %>%
  select(workflow_id = id, start_date, end_date, notes) %>%
  collect() %>%
  mutate(configs_df = map(notes, parse_notes),
         workflow_id = as.numeric(workflow_id)) %>%
  unnest(configs_df) %>%
  select(-notes)

workflows_years = expand.grid(
  workflow_id = as.numeric(pull(workflow_df, workflow_id)),
  year = seq(1902, 1912)
) %>%
  as_tibble() %>%
  mutate(startmonth = case_when(year == 1902 ~ 6, TRUE ~ 1))

plan <- drake_plan(
  lai_raw = target(
    possibly(get_monthly_lai, NULL)(workflow_id, year, startmonth),
    transform = map(!!!workflows_years)
  ),
  lai_data = target(
    bind_rows(lai_raw),
    transform = combine(lai_raw)
  ),
  lai_results = workflow_df %>%
    select(-start_date, -end_date) %>%
    left_join(lai_data) %>%
    filter(!is.na(lai)) %>%
    group_by(month, workflow_id) %>%
    mutate(total_lai = sum(lai)) %>%
    ungroup(month, workflow_id) %>%
    rename(rtm = multiple_scatter) %>%
    mutate(
      rtm = if_else(rtm, "multiple scatter", "two-stream") %>%
        factor(c("two-stream", "multiple scatter")),
      crown_model = if_else(crown_model, "gap", "complete shading") %>%
        factor(c("complete shading", "gap")),
      n_limitation = case_when(n_limit_ps & n_limit_soil ~ "both",
                               n_limit_ps ~ "plant",
                               n_limit_soil ~ "decomp",
                               TRUE ~ "none") %>%
        factor(c("both", "plant", "decomp", "none")) %>%
        fct_relabel(~paste("N limit:", .x)),
      trait_plasticity = if_else(trait_plasticity,
                                 "static traits", "plastic traits") %>%
        factor(c("static traits", "plastic traits"))
    ),
  lai_results_fst = write_fst(
    lai_results,
    file_out(!!(
      here("analysis", "data", "derived-data",
           paste0("results-", created_since)) %>%
        dir_create() %>%
        path("lai_results.fst")
    ))
  ),
  results_plot = ggplot(lai_results) +
    aes(x = month, y = lai, color = pft) +
    geom_line() +
    facet_grid(rows = vars(crown_model, rtm),
               cols = vars(trait_plasticity, n_limitation)) +
    labs(y = "Leaf area index", color = "PFT") +
    scale_x_datetime(date_breaks = "2 years",
                     date_labels = "%Y") +
    cowplot::theme_cowplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title.x = element_blank()),
  results_plot_png = save_plot(file_out(!!(
    here("analysis", "figures", created_since) %>%
      dir_create() %>%
      path("lai_10year.png")
  )), results_plot, base_width = 10, base_height = 7),
  paper = target(
    rmarkdown::render(
    file_in(!!(here("analysis", "paper", "paper.Rmd"))),
    "github_document"
  ), trigger = trigger(condition = !file.exists("analysis/paper/paper.md")) )
)

future::plan(future.callr::callr)
dconf <- drake_config(plan)
make(plan, parallelism = "future", jobs = availableCores())
