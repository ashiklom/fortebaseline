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

suppressPackageStartupMessages({
  library(drake)
  # Data processing
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(purrr)
  library(lubridate)
  library(readr)
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
  library(fortebaseline)
})

cmdargs <- commandArgs(trailingOnly = TRUE)

# Set common directories and paths
analysis_dir <- dir_create(here("analysis"))
data_dir <- dir_create(path(analysis_dir, "data"))
download_dir <- dir_create(path(data_dir, "retrieved"))

monthly_output_file <- path(download_dir, "monthly-ensemble-output.fst")
meta_analysis_file <- path(download_dir, "meta-analysis.rds")
ensemble_params_file <- path(download_dir, "ed-ensemble-params.fst")
cohort_file <- path(download_dir, "cohort_output.fst")

cohort_osf <- "2af5g"
params_osf <- "pcrav"
get_timestamp <- function(osf_id) {
  osfr::osf_retrieve_file(osf_id) %>%
    dplyr::pull(meta) %>%
    purrr::pluck(1, "attributes", "date_modified")
}

plan <- drake_plan(
  #########################################
  # Common elements
  #########################################
  paper = target(
    rmarkdown::render(
      knitr_in(!!(path(analysis_dir, "paper", "paper.Rmd"))),
      .format
    ),
    transform = map(.format = c("html_document"))),
  param_table = file_in(!!(path(data_dir, "derived-data", "parameter-table.csv"))) %>%
    read_csv(),
  #########################################
  # Summary time series
  #########################################
  cohort_file_dl = target(
    download.file(file.path("https://osf.io/download/", cohort_osf),
                  file_out(!!cohort_file)),
    trigger = trigger(change = get_timestamp(cohort_osf))
  ),
  variable_cols = c("workflow_id", "run_id", "datetime", "pft", "nplant",
                    "bleaf", "bsapwooda", "bstorage",
                    "fmean_gpp_co", "fmean_npp_co", "lai_co"),
  use_workflows = fst(file_in(!!cohort_file))[, c("workflow_id", "run_id")] %>%
    unique() %>%
    count(workflow_id) %>%
    filter(n >= 4) %>%
    pull(workflow_id),
  plot_means = fst(file_in(!!cohort_file)) %>%
    .[, variable_cols] %>%
    as_tibble() %>%
    filter(workflow_id %in% use_workflows) %>%
    semi_join(current_workflows, by = "workflow_id") %>%
    group_by(workflow_id, run_id, datetime) %>%
    summarize(
      agb = sum((bleaf + bsapwooda + bstorage) * nplant),
      gpp = sum(fmean_gpp_co * nplant),
      npp = sum(fmean_npp_co * nplant),
      lai = sum(lai_co)
    ) %>%
    ungroup(),
  jja_means = plot_means %>%
    filter(month(datetime) %in% 6:8) %>%
    group_by(workflow_id, run_id, year = year(datetime)) %>%
    summarize_at(vars(-datetime), mean) %>%
    ungroup() %>%
    inner_join(current_workflows, by = "workflow_id"),
  jja_long = jja_means %>%
    gather(variable, value, agb:lai) %>%
    mutate(variable = factor(variable, c("gpp", "npp", "agb", "lai"))),
  jja_summary = jja_long %>%
    group_by(model_split, color, variable, year) %>%
    summarize(lo = quantile(value, 0.1),
              hi = quantile(value, 0.9)),
  my_labeller = labeller(
    variable = as_labeller(c(
      "gpp" = "GPP ~ (kgC ~ year^{-1})",
      "npp" = "NPP ~ (kgC ~ year^{-1})",
      "agb" = "AGB ~ (kgC)",
      "lai" = "LAI"
    ), default = label_parsed),
    .default = label_value
  ),
  hardiman = tribble(
    ~variable, ~low, ~mean, ~hi,
    "lai", 1.8, 4.14, 6.56,
    "npp", 1.68, 3.11, 7.26
  ) %>% mutate(variable = factor(variable, c("gpp", "npp", "lai", "agb"))),
  summary_ts_plot = ggplot(jja_long) +
    aes(x = year, y = value, group = run_id) +
    geom_line(color = "grey50", alpha = 0.5) +
    geom_ribbon(aes(ymin = lo, ymax = hi, y = NULL, group = NULL,
                    fill = color),
                data = jja_summary, alpha = 0.7) +
    geom_hline(aes(yintercept = mean), color = "black", linetype = "solid",
               data = hardiman) +
    geom_hline(aes(yintercept = low), color = "black", linetype = "dashed",
               data = hardiman) +
    geom_hline(aes(yintercept = hi), color = "black", linetype = "dashed",
               data = hardiman) +
    scale_fill_identity() +
    facet_grid(vars(variable), vars(model_split), scales = "free_y",
               switch = "y", labeller = my_labeller) +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.placement = "outside",
          strip.background = element_blank()),
  #########################################
  # Parameter distributions
  #########################################
  meta_analysis_dl = target(
    download.file(file.path("https://osf.io/download", params_osf),
                  file_out(!!meta_analysis_file)),
    trigger = trigger(change = get_timestamp(params_osf))
  ),
  ma_posterior = file_in(!!meta_analysis_file) %>%
    readRDS() %>%
    tidy_posterior() %>%
    mutate(is_posterior = TRUE),
  ma_prior = file_in(!!path(data_dir, "derived-data", "pft-priors.csv")) %>%
    read_csv() %>%
    select(bety_name = pft, one_of(colnames(ma_posterior))) %>%
    left_join(pfts(), by = "bety_name"),
  missing_posteriors = ma_prior %>%
    anti_join(ma_posterior, by = c("pft", "trait")) %>%
    draw_traits() %>%
    mutate(is_posterior = FALSE),
  trait_distribution = ma_posterior %>%
    bind_rows(missing_posteriors),
  param_dist_gg = trait_distribution %>%
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
  #########################################
  # LAI by PFT plot
  #########################################
  cohort_lai = fst(file_in(!!cohort_file)) %>%
    .[, c("workflow_id", "run_id", "datetime", "pft", "lai_co")] %>%
    as_tibble() %>%
    filter(workflow_id %in% use_workflows) %>%
    semi_join(current_workflows, by = "workflow_id"),
  lai_q90 = cohort_lai %>%
    group_by(workflow_id, run_id, year = year(datetime), pft) %>%
    summarize(lai = quantile(lai_co, 0.9)) %>%
    ungroup() %>%
    rename(num = pft) %>%
    left_join(pfts(), by = "num"),
  run_groups = lai_q90 %>%
    select(workflow_id, run_id, year, pft, lai) %>%
    group_by(workflow_id) %>%
    group_modify(create_run_groups),
  use_runs = run_groups %>%
    group_by(workflow_id, cluster) %>%
    slice(1),
  lai_pft_plot = lai_q90 %>%
    inner_join(use_runs, by = c("workflow_id", "run_id")) %>%
    inner_join(current_workflows, by = "workflow_id") %>%
    ggplot() +
    aes(x = year, y = lai, color = pft) +
    geom_line() +
    facet_grid(vars(cluster), vars(model_split)) +
    scale_color_manual(values = pfts("color")) +
    labs(y = "Leaf area index", color = "PFT") +
    theme_cowplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.text.y = element_blank()
    ),
  #########################################
  # Parameter vs. structure uncertainty 
  #########################################
  time_averages = jja_means %>%
    filter(year > 1910) %>%
    group_by(model, color, run_id) %>%
    summarize_at(vars(gpp, npp, agb, lai), mean),
  parameter_uncertainty = time_averages %>%
    group_by(model, color) %>%
    summarize_at(vars(gpp, npp, agb, lai), var) %>%
    ungroup() %>%
    arrange(model),
  structure_uncertainty = time_averages %>%
    group_by(model) %>%
    summarize_at(vars(gpp, npp, agb, lai), mean) %>%
    ungroup() %>%
    summarize_at(vars(-model), var) %>%
    mutate(model = "Across-structure",
           color = "black"),
  both_uncertainty = parameter_uncertainty %>%
    mutate(model = as.character(model)) %>%
    bind_rows(structure_uncertainty) %>%
    mutate(model = fct_inorder(model)),
  within_across_plot = both_uncertainty %>%
    gather(variable, variance, -model, -color) %>%
    mutate(variable = factor(variable, c("gpp", "npp", "lai", "agb")) %>%
             fct_recode("GPP" = "gpp",
                        "NPP" = "npp",
                        "LAI" = "lai",
                        "AGB" = "agb")) %>%
    ggplot() +
    aes(x = model, y = variance, fill = model) +
    geom_col() +
    facet_wrap(vars(variable), scales = "free_y") +
    labs(x = "Model structure", y = "Variance") +
    scale_fill_manual(
      values = tibble::deframe(both_uncertainty[, c("model", "color")])
    ) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  #########################################
  # Sensitivity analysis
  #########################################
  ## run_params = ...
  ## run_params_wide = ...,
  ## sensitivity_inputs = time_averages %>%
  ##   left_join()
)

old_plan <- drake_plan(
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
    separate(xvar, c("pft", "trait"), sep = "\\.\\.") %>%
    mutate(pft = factor(pft, pfts("shortname"))),
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
  )
)

if ("--poster" %in% cmdargs) source("analysis/drake_poster.R")

# Parallelism configuration. Not sure which of these is better...
future::plan(future.callr::callr) # <-- Currently throws error
## future::plan(future::multiprocess)

dconf <- drake_config(
  plan,
  parallelism = "future",
  jobs = availableCores()
)

if (interactive()) {
  callr::rscript("analysis/drake.R")
} else {
  # Probably called from a script 
  message("Called from script. Running `drake::make`.")
  make(config = dconf)
}
