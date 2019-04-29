#!/usr/bin/env Rscript
if (getRversion() >= "3.6") {
  options(conflicts.policy = "strict")
  conflictRules("testthat", exclude = "matches")
  conflictRules("drake", exclude = c("gather", "expand", "plan"))
  conflictRules("dplyr",
                mask.ok = c("filter", "lag", "intersect",
                            "setdiff", "setequal", "union"))
  conflictRules("lubridate",
                mask.ok = c("as.difftime", "date"),
                exclude = c("intersect", "setdiff", "union", "here"))
  conflictRules("ggplot2", exclude = "ggsave")
} else {
  warning("Package conflict resolution requires R >= 3.6. ",
          "This script may not work as expected.",
          immediate. = TRUE)
}

library(drake)
# Data processing
library(dplyr)
library(tidyr)
library(forcats)
library(purrr)
library(lubridate)
# Parallel execution
library(future)
library(future.callr)
library(furrr)
# Files and file paths
library(fs)
library(fst)
library(here)
# Plotting
library(ggplot2)
library(cowplot)

## library(fortebaseline)

devtools::load_all(here::here(), attach_testthat = FALSE)
expose_imports("fortebaseline")

# Set common directories and paths
analysis_dir <- dir_create(here("analysis"))
data_dir <- dir_create(path(analysis_dir, "data"))
download_dir <- dir_create(path(data_dir, "retrieved"))

monthly_output_file <- path(download_dir, "monthly-ensemble-output.fst")
meta_analysis_file <- path(download_dir, "meta-analysis.rds")
ensemble_params_file <- path(download_dir, "ed-ensemble-params.fst")

plan <- drake_plan(
  # Common elements
  paper = target(
    rmarkdown::render(
      knitr_in(!!(path(analysis_dir, "paper", "paper.Rmd"))),
      "github_document"
    )),
  model_scale = c(RColorBrewer::brewer.pal(
    n = nrow(both_uncertainty) - 1,
    name = "Paired"
  ), "black") %>% setNames(both_uncertainty[["model"]]),
  #######################################
  ## Figure 1: Parameter distributions ##
  #######################################
  meta_analysis_dl = target(
    download.file("https://osf.io/download/pcrav",
                  file_out(!!meta_analysis_file)),
    trigger = trigger(condition = !file_exists(meta_analysis_file),
                      mode = "condition")
  ),
  ma_posterior = readRDS(file_in(!!meta_analysis_file)) %>%
    tidy_posterior(),
  param_dist_gg = ma_posterior %>%
    mutate(pft = factor(pft, pfts("pft"))) %>%
    unnest(draws) %>%
    filter(draws < quantile(draws, 0.975),
           draws > quantile(draws, 0.025)) %>%
    ggplot() +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin() +
    facet_wrap(vars(trait), scales = "free_y") +
    scale_fill_manual(values = pfts("color")) +
    labs(x = "PFT", fill = "PFT") +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank()),
  ########################################
  ## Figure 2: Aggregate variables plot ##
  ########################################
  monthly_output_dl = target( 
    download.file("https://osf.io/download/3twxa",
                  file_out(!!monthly_output_file)),
    trigger = trigger(condition = !file_exists(monthly_output_file),
                      mode = "condition")
  ),
  raw_monthly_output = read_fst(file_in(!!monthly_output_file)) %>%
    as_tibble() %>%
    mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
    select(workflow_id, everything()) %>%
    select(-workflows) %>%
    left_join(workflow_structures(), by = "workflow_id"),
  hardiman = tribble(
    ~variable, ~low, ~mean, ~hi,
    "LAI", 1.8, 4.14, 6.56,
    "NPP", 1.68, 3.11, 7.26
  ) %>% mutate(variable = factor(variable, c("GPP", "NPP", "LAI", "AGB"))),
  monthly_means = raw_monthly_output %>%
    group_by(workflow_id, crown, rtm, traits, date, runs, pft) %>%
    select(group_cols(), starts_with("mmean"), agb_py) %>%
    # Remove duplicated values -- `mmean` values should be unique by PFT.
    summarize_all(mean),
  monthly_means_site = monthly_means %>%
    select(
      group_cols(),
      GPP = mmean_gpp_py,
      NPP = mmean_npp_py,
      LAI_pft = mmean_lai_py,
      AGB_pft = agb_py
    ) %>%
    group_by(crown, rtm, traits, date, runs) %>%
    # Aggregate PFTs
    summarize(
      GPP = mean(GPP),
      NPP = mean(NPP),
      LAI = sum(LAI_pft),
      AGB = sum(AGB_pft)
    ) %>%
    mutate(
      year = floor_date(date, "years") %m+% period(6, "months"),
      month = month(date)
    ),
  summary_ts_plot = monthly_means_site %>%
    filter(month %in% 7:9) %>%
    tidyr::gather(variable, value, GPP:AGB) %>%
    mutate(variable = fct_inorder(variable)) %>%
    # Aggregate runs
    group_by(crown, rtm, traits, variable, year) %>%
    summarize(
      mean = mean(value),
      lo = quantile(value, 0.2),
      hi = quantile(value, 0.8)
    ) %>%
    filter(!is.na(variable)) %>%
    mutate(model = interaction(crown, rtm, traits, sep = "\n")) %>%
    ggplot() +
    aes(x = year, y = mean, ymin = lo, ymax = hi) +
    geom_line() +
    geom_ribbon(alpha = 0.35) +
    geom_hline(aes(yintercept = mean), data = hardiman, linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = low), data = hardiman, linetype = "dotted", color = "red") +
    geom_hline(aes(yintercept = hi), data = hardiman, linetype = "dotted", color = "red") +
    facet_grid(vars(variable), vars(model), scales = "free_y", switch = "y") +
    scale_color_manual(values = unname(head(model_scale, -1))) +
    scale_fill_manual(values = unname(head(model_scale, -1))) +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.placement = "outside",
          strip.background.y = element_blank()),
  ##############################
  ## Figure 3: Big facet plot ##
  ##############################
  bigfacet_plot = monthly_means %>%
    ungroup() %>%
    select(crown, rtm, traits, date, pft, runs,
           mmean_lai_py) %>%
    filter(month(date) == 7) %>%
    left_join(
      # Order runs in order of increasing max Pine LAI
      monthly_means %>%
        ungroup() %>%
        select(workflow_id, runs, pft, mmean_lai_py) %>%
        filter(pft == "Pine") %>%
        group_by(workflow_id, runs) %>%
        summarize(pine_lai = max(mmean_lai_py)) %>%
        mutate(run_i = rank(pine_lai)),
      by = "runs") %>%
    mutate(model = interaction(crown, rtm, traits, sep = "\n")) %>%
    ggplot() +
    aes(x = date, y = mmean_lai_py, color = pft) +
    geom_line() +
    facet_grid(vars(run_i), vars(model), drop = TRUE) +
    labs(y = "Leaf area index", color = "PFT") +
    scale_color_manual(values = pfts("color")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_cowplot() +
    theme(strip.text.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x = element_blank()),
  #####################################
  ## Figure 4: Parameter sensitivity ##
  #####################################
  run_params_dl = target( 
    download.file("https://osf.io/download/f5vpd",
                  file_out(!!ensemble_params_file)),
    trigger = trigger(condition = !file_exists(ensemble_params_file),
                      mode = "condition")
  ),
  run_params_all = read_fst(file_in(!!ensemble_params_file)) %>%
    as_tibble(),
  meta_vars = run_params_all %>%
    select(-workflow_id, -path) %>%
    group_by(pft) %>%
    summarize_all(~length(unique(.x))) %>%
    tidyr::gather(variable, count, -pft, -run_id) %>%
    filter(count > 1) %>%
    distinct(variable) %>%
    pull(),
  run_params = run_params_all %>%
    select(workflow_id, run_id, pft, meta_vars),
  run_params_wide = run_params %>%
    mutate(pft = lvls_revalue(pft, pfts("shortname"))) %>%
    tidyr::gather(param, value, -workflow_id, -run_id, -pft) %>%
    unite(pft_param, pft, param, sep = "..") %>%
    spread(pft_param, value),
  growing_season_averages = monthly_means_site %>%
    filter(date > "1910-01-01", month %in% 7:9) %>%
    group_by(crown, rtm, traits, run_id = as.numeric(runs)) %>%
    summarize_at(vars(GPP:AGB), mean),
  sensitivity_inputs = growing_season_averages %>%
    left_join(run_params_wide, by = "run_id"),
  sensitivity_results = sensitivity_inputs %>%
    ungroup() %>%
    tidyr::gather(yvar, yvalue, GPP:AGB) %>%
    tidyr::gather(xvar, xvalue, -(crown:workflow_id), -yvar, -yvalue) %>%
    group_by_at(vars(workflow_id, crown:traits, xvar, yvar)) %>%
    summarize(raw_sa = list(possibly(sensitivity_analysis, NULL)(yvalue, xvalue))) %>%
    filter(map_lgl(raw_sa, negate(is.null))) %>%
    unnest(raw_sa),
  sensitivity_plot_data = sensitivity_results %>%
    mutate(model = interaction(crown, rtm, traits, sep = "\n"),
           elasticity = pmax(elasticity, -200),
           cv = pmin(cv, 1000)) %>%
    separate(xvar, c("pft", "trait"), sep = "\\.\\."),
  sensitivity_plot_piece = target(
    ggplot(sensitivity_plot_data) +
      aes(x = pft, y = trait, fill = YYY) +
      geom_tile() +
      facet_grid(vars(yvar), vars(model)) +
      SCALE +
      labs(x = "PFT", y = "Trait") +
      theme_cowplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = rel(0.6)),
            legend.position = "bottom"),
    transform = map(YYY = c(elasticity, pvar),
                    SCALE = c(scale_fill_gradient2(),
                              scale_fill_continuous(low = "white", high = "blue")))
  ),
  sensitivity_plot = target(
    cowplot::plot_grid(sensitivity_plot_piece),
    transform = combine(sensitivity_plot_piece)
  ),
  #######################################
  ## Within vs. across model structure ##
  #######################################
  parameter_uncertainty = growing_season_averages %>%
    group_by(crown, rtm, traits) %>%
    summarize_at(vars(GPP:AGB), var) %>%
    ungroup() %>%
    mutate(model = as.character(interaction(crown, rtm, traits))),
  structure_uncertainty = growing_season_averages %>%
    group_by(crown, rtm, traits) %>%
    summarize_at(vars(GPP:AGB), mean) %>%
    ungroup() %>%
    summarize_at(vars(GPP:AGB), var) %>%
    mutate(model = "Across-structure"),
  both_uncertainty = parameter_uncertainty %>%
    select(model, GPP:AGB) %>%
    bind_rows(structure_uncertainty) %>%
    mutate(model = fct_inorder(model)),
  within_across_plot = both_uncertainty %>%
    gather(variable, variance, GPP:AGB) %>%
    ggplot() +
    aes(x = model, y = variance, fill = model) +
    geom_col() +
    facet_wrap(vars(variable), scales = "free_y") +
    scale_fill_manual(values = model_scale) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
)

# Parallelism configuration. Not sure which of these is better...
## future::plan(future.callr::callr) # <-- Currently throws error
future::plan(future::multiprocess)

dconf <- drake_config(
  plan,
  parallelism = "future",
  jobs = availableCores(),
  ## jobs = 1,
  prework = paste0("devtools::load_all(",
                   "here::here(), ",
                   "quiet = TRUE, ",
                   "attach_testthat = FALSE)")
)

if (interactive()) {
  callr::rscript("analysis/drake.R")
} else {
  # Probably called from a script 
  message("Called from script. Running `drake::make`.")
  make(config = dconf)
}
