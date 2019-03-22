library(tidyverse)
library(data.table)
library(furrr)
library(fortebaseline)
library(cowplot)

plan(list(tweak(multiprocess, workers = 8)))

con <- default_connection()

bety_workflows <- tbl(con, "workflows") %>%
  filter(start_date == "1902-06-01",
         end_date == "1912-12-31",
         created_at > "2019-03-20")

workflow_df <- bety_workflows %>%
  select(workflow_id = id, start_date, end_date, notes) %>%
  collect() %>%
  mutate(configs_df = map(notes, parse_notes)) %>%
  unnest(configs_df) %>%
  select(-notes)

workflow_ids <- pull(workflow_df, workflow_id)

workflows_years <- expand.grid(
  workflow_id = workflow_ids,
  year = seq(1902, 1912)
) %>%
  as_tibble() %>%
  mutate(startmonth = case_when(year == 1902 ~ 6, TRUE ~ 1))

lai_data <- future_pmap_dfr(
  workflows_years,
  possibly(get_monthly_lai, NULL),
  .progress = TRUE
)

lai_results <- workflow_df %>%
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
    trait_plasticity = if_else(trait_plasticity, "static traits", "plastic traits") %>%
      factor(c("static traits", "plastic traits"))
  )

outdir <- here::here("analysis", "data", "derived-data",
                     paste0("results-", strftime(Sys.Date())))
dir.create(outdir, showWarnings = FALSE)

fst::write_fst(lai_results, file.path(outdir, "ed-decade-lai-output.fst"))

results_plot <- ggplot(lai_results) +
  aes(x = month, y = lai, color = pft) +
  geom_line() +
  facet_grid(rows = vars(crown_model, rtm),
             cols = vars(trait_plasticity, n_limitation)) +
  labs(y = "Leaf area index", color = "PFT") +
  scale_x_datetime(date_breaks = "2 years",
                   date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())
results_plot

plotfile <- here::here("analysis", "figures", Sys.Date(), "lai_10year.png")
dir.create(dirname(plotfile), showWarnings = TRUE)

save_plot(plotfile, results_plot, base_width = 10, base_height = 7)
