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
                exclude = c("intersect", "setdiff", "union", "here",
                            "stamp"))
  conflictRules("ggplot2", exclude = "ggsave")
  conflictRules("data.table", exclude = c("between", "first", "last",
                                          "transpose", "hour", "isoweek",
                                          "mday", "minute", "month",
                                          "quarter", "second", "wday", "week",
                                          "yday", "year"))
} else {
  warning("Package conflict resolution requires R >= 3.6. ",
          "This script may not work as expected.",
          immediate. = TRUE)
}

suppressPackageStartupMessages({
  library(drake)
  # Data processing
  library(data.table)
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

# "Observation time" output files
cohort_file <- path(download_dir, "all-output-cohort.fst")
cohort_osf <- "d4u2j"
pft_file <- path(download_dir, "all-output-pft.fst")
pft_osf <- "5dht2"
scalar_file <- path(download_dir, "all-output-scalar.fst")
scalar_osf <- "fgdkm"
soil_file <- path(download_dir, "all-output-soil.fst")
soil_osf <- "4gjpy"

# Monthly output files
mcohort_file <- path(download_dir, "all-cohort-monthly-output.fst")
mcohort_osf <- "..."
mpft_file <- path(download_dir, "all-output-monthly-pft.fst")
mpft_osf <- "..."
mscalar_file <- path(download_dir, "all-output-monthly-scalar.fst")
mscalar_osf <- "..."
msoil_file <- path(download_dir, "all-output-monthly-soil.fst")
msoil_osf <- "..."

ensemble_params_file <- path(download_dir, "input-parameters.csv")
params_osf <- "87ku4"

trait_distribution_file <- path(download_dir, "trait-distribution.rds")
td_osf <- "bfyuh"

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
  param_table = file_in(!!(path(data_dir, "derived-data",
                                "parameter-table.csv"))) %>%
    read_csv(),
  #########################################
  # File downloads
  #########################################
  ## cohort_file_dl = target(
  ##   download.file(file.path("https://osf.io/download/", cohort_osf),
  ##                 file_out(!!cohort_file)),
  ##   trigger = trigger(change = get_timestamp(cohort_osf),
  ##                     condition = !file.exists(cohort_file))
  ## ),
  ## run_params_dl = target(
  ##   download.file(file.path("https://osf.io/download/", params_osf),
  ##                 file_out(!!ensemble_params_file)),
  ##   trigger = trigger(change = get_timestamp(params_osf))
  ## ),
  ## trait_distribution_dl = target(
  ##   download.file(file.path("https://osf.io/download", td_osf),
  ##                 file_out(!!trait_distribution_file)),
  ##   trigger = trigger(change = get_timestamp(td_osf))
  ## ),
  #########################################
  # Summary time series
  #########################################
  variable_cols = c("case", "datetime", "pft", "nplant",
                    "bleaf", "bsapwooda", "bstorage",
                    "fmean_gpp_co", "fmean_npp_co", "lai_co"),
  cases = fst(!!cohort_file)[, "case", drop = FALSE] %>%
    distinct() %>%
    mutate(
      param_id = as.numeric(substring(case, 1, 3)),
      model_id = substring(case, 4, 6),
      crown = fct_recode(substring(model_id, 1, 1),
                         "closed" = "C", "finite" = "F") %>%
        fct_relevel("closed", "finite"),
      rtm = fct_recode(substring(model_id, 2, 2),
                       "multi-scatter" = "M", "two-stream" = "T") %>%
        fct_relevel("two-stream", "multi-scatter"),
      traits = fct_recode(substring(model_id, 3, 3),
                          "static" = "S", "plastic" = "P") %>%
        fct_relevel("static", "plastic"),
      model = interaction(crown, rtm, traits, sep = " ")
    ) %>%
    as_tibble(),
  models = distinct(cases, model_id, model, crown, rtm, traits) %>%
    arrange(model) %>%
    mutate(color = RColorBrewer::brewer.pal(n(), "Paired")),
  model_colors = tibble::deframe(models[, c("model", "color")]),
  use_vars = c("gpp", "npp", "agb", "lai", "shannon"),
  use_vars_cap = c("GPP", "NPP", "AGB", "LAI", "Shannon"),
  plot_means = setDT(fst(!!cohort_file)[, variable_cols])[j = .(
    agb = sum((bleaf + bsapwooda + bstorage) * nplant),
    gpp = sum(fmean_gpp_co * nplant),
    npp = sum(fmean_npp_co * nplant),
    lai = sum(lai_co)
  ), by = c("case", "datetime")] %>%
    as_tibble() %>%
    mutate(model_id = substr(case, 4, 6)),
  # Calculate diversity indices based on LAI
  diversity = as.data.table(fst(cohort_file)[, c("case", "datetime",
                                                   "pft", "lai_co")]) %>%
    .[, year := year(datetime)] %>%
    .[, .(lai_co = quantile(lai_co, 0.9)), .(case, year, pft)] %>%
    .[, lai_p := lai_co / sum(lai_co), .(case, year)] %>%
    .[lai_p > 0] %>%
    .[, .(shannon = -sum(lai_p * log(lai_p))), .(case, year)] %>%
    as_tibble() %>%
    mutate(model_id = substr(case, 4, 6)),
  jja_means = plot_means %>%
    filter(month(datetime) %in% 6:8) %>%
    group_by(case, model_id, year = year(datetime)) %>%
    summarize_at(vars(-datetime), mean) %>%
    ungroup() %>%
    left_join(diversity, by = c("case", "model_id", "year")) %>%
    mutate(shannon = if_else(is.na(shannon), 0, shannon)) %>%
    inner_join(models, by = "model_id"),
  jja_long = jja_means %>%
    gather(variable, value, agb:shannon) %>%
    mutate(variable = factor(variable, use_vars),
           param_id = as.numeric(substr(case, 1, 3))),
  jja_summary = jja_long %>%
    group_by(model, color, variable, year) %>%
    summarize(avg = mean(value),
              lo = quantile(value, 0.1),
              hi = quantile(value, 0.9)),
  my_labeller = labeller(
    model = function(x) gsub(" ", "\n", x),
    variable = as_labeller(c(
      "gpp" = "GPP ~ (MgC ~ ha^-1 ~ year^-1)",
      "npp" = "NPP ~ (MgC ~ ha^-1 ~ year^-1)",
      "agb" = "AGB ~ (kgC)",
      "lai" = "LAI",
      "shannon" = "Shannon ~ diversity"
    ), default = label_parsed),
    .default = label_value
  ),
  observations = tribble(
    ~variable, ~low, ~mean, ~hi, ~source,
    ## "lai", 1.8, 4.14, 6.56, "Hardiman 2013",
    ## "npp", 1.68, 3.11, 7.26, "Hardiman 2013",
    "npp", 1.69 - 1.96 * 0.512, 1.69, 1.69 + 1.96 * 0.512, "UMBS",
    "lai", 3.97 - 1.96 * 0.423, 3.97, 3.97 + 1.96 * 0.423, "Ameriflux"
  ) %>% mutate(variable = factor(variable, use_vars),
               year = max(jja_summary$year)),
  observations_wide = observations %>%
    select(-year, -source) %>%
    gather(stat, value, low, mean, hi) %>%
    unite("variable", variable, stat) %>%
    spread(variable, value),
  summary_ts_plot = ggplot(jja_long) +
    aes(x = year) +
    geom_line(aes(y = value, group = param_id, color = color), alpha = 0.25) +
    geom_line(data = jja_summary, aes(y = hi),
              color = "black", linetype = "dashed") +
    geom_line(data = jja_summary, aes(y = lo),
              color = "black", linetype = "dashed") +
    geom_line(aes(y = avg), color = "black", size = 1, data = jja_summary) +
    geom_pointrange(data = observations,
                    aes(y = mean, ymin = low, ymax = hi),
                    color = "black") +
    scale_color_identity() +
    facet_grid(vars(variable), vars(model), scales = "free_y",
               switch = "y", labeller = my_labeller) +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.placement = "outside",
          strip.background = element_blank()),
  #########################################
  # Parameter distributions
  #########################################
  trait_distribution = readRDS(file_in(!!trait_distribution_file)),
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
    .[, c("case", "datetime", "pft", "lai_co")] %>%
    as_tibble(),
  lai_q90 = setDT(fst(file_in(!!cohort_file))[, j = c(
    "case", "datetime", "pft", "lai_co")]) %>%
    .[, model_id := substr(case, 4, 6)] %>%
    .[, .(lai = quantile(lai_co, 0.9)),
      .(case, model_id, year = year(datetime), pft)] %>%
    as_tibble() %>%
    rename(num = pft) %>%
    left_join(pfts(), by = "num"),
  run_groups = lai_q90 %>%
    select(case, model_id, year, pft, lai) %>%
    group_by(model_id) %>%
    group_modify(create_run_groups),
  use_runs = run_groups %>%
    group_by(model_id, cluster) %>%
    slice(1),
  lai_pft_plot = lai_q90 %>%
    inner_join(models, by = "model_id") %>%
    inner_join(use_runs, by = c("model_id", "case")) %>%
    ggplot() +
    aes(x = year, y = lai, color = pft) +
    geom_line() +
    facet_grid(vars(cluster), vars(model), labeller = my_labeller) +
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
    filter(year > 1975) %>%
    group_by(model, color, case) %>%
    summarize_at(use_vars, mean) %>%
    mutate(param_id = as.numeric(substr(case, 1, 3))),
  parameter_uncertainty = time_averages %>%
    group_by(model, color) %>%
    summarize_at(use_vars, var) %>%
    ungroup() %>%
    arrange(model),
  structure_uncertainty = time_averages %>%
    group_by(model) %>%
    summarize_at(use_vars, mean) %>%
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
    mutate(variable = factor(variable, use_vars) %>%
             lvls_revalue(use_vars_cap)) %>%
    ggplot() +
    aes(x = model, y = variance, fill = model) +
    geom_col() +
    facet_wrap(vars(variable), scales = "free_y") +
    labs(x = "Model structure", y = "Variance") +
    scale_fill_manual(
      values = tibble::deframe(both_uncertainty[, c("model", "color")])
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank()
    ),
  #########################################
  # Sensitivity analysis
  #########################################
  run_params = file_in(!!ensemble_params_file) %>%
    read_csv(col_types = cols(name = col_character(),
                              .default = col_double())) %>%
    rename(bety_name = name) %>%
    left_join(pfts() %>%
                select(bety_name, pft, shortname),
              by = "bety_name"),
  sensitivity_inputs = run_params %>%
    inner_join(time_averages, by = "param_id") %>%
    gather(yvar, yvalue, one_of(use_vars)) %>%
    select(-color, -case, -bety_name, -pft) %>%
    gather(xvar, xvalue,
           -param_id, -shortname,
           -model, -yvar, -yvalue),
  sensitivity_results = sensitivity_inputs %>%
    group_by(model, shortname, xvar, yvar) %>%
    summarize(raw_sa = list(possibly(sensitivity_analysis, NULL)(yvalue, xvalue))) %>%
    filter(map_lgl(raw_sa, negate(is.null))) %>%
    unnest(raw_sa) %>%
    ungroup(),
  sensitivity_plot_data = sensitivity_results %>%
    mutate(elasticity = if_else(abs(elasticity) > 75,
                                75 * sign(elasticity),
                                elasticity)) %>%
    group_by(model, yvar) %>%
    mutate(fpvar = pvar / sum(pvar)) %>%
    group_by(xvar) %>%
    mutate(total_pvar = sum(pvar)) %>%
    ungroup() %>%
    mutate(
      trait_alpha = factor(xvar) %>% fct_rev(),
      trait_pvar = fct_reorder(factor(xvar), total_pvar),
      model = fct_relabel(model, gsub, pattern = " ", replacement = "\n"),
      shortname = factor(shortname, pfts("shortname")),
      yvar = factor(yvar, use_vars) %>%
        lvls_revalue(use_vars_cap)
    ),
  sensitivity_plot_piece = target(
    ggplot(sensitivity_plot_data) +
      aes(x = shortname, y = trait_alpha, fill = YYY) +
      geom_tile() +
      facet_grid(vars(yvar), vars(model)) +
      SCALE +
      labs(x = "PFT", y = "Trait") +
      theme_cowplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            axis.text.y = element_text(size = rel(0.6)),
            legend.position = "bottom",
            legend.key.width = unit(3, "lines")),
    transform = map(YYY = c(elasticity, fpvar),
                    SCALE = c(scale_fill_gradientn(colors = c("darkred", "darkorange3",
                                                              "white",
                                                              "deepskyblue3", "darkblue"),
                                                   values = scales::rescale(c(
                                                     -75, -25,
                                                     0,
                                                     25, 75))),
                              scale_fill_continuous(low = "white", high = "darkblue")))
  ),
  sensitivity_plot = target(
    cowplot::plot_grid(sensitivity_plot_piece),
    transform = combine(sensitivity_plot_piece)
  ),
  top_params_fpvar = sensitivity_plot_data %>%
    group_by(yvar, model) %>%
    top_n(5, fpvar) %>%
    arrange(desc(fpvar), .by_group = TRUE) %>%
    ungroup() %>%
    select(model, shortname, yvar, xvar, fpvar),
  top_params_fpvar_count = top_params_fpvar %>%
    count(yvar, shortname, xvar, sort = TRUE),
  #########################################
  # Pairs plot of time-averaged values
  #########################################
  pairs_time_averaged = time_averages %>%
    ungroup() %>%
    ggplot() +
    aes(x = lai, y = npp, color = model) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_rug(color = "black", alpha = 0.5) +
    geom_point(data = observations_wide,
               aes(x = lai_mean, y = npp_mean),
               size = 3,
               shape = 17,
               color = "black",
               inherit.aes = FALSE) +
    geom_rect(data = observations_wide,
              aes(xmin = lai_low, xmax = lai_hi,
                  ymin = npp_low, ymax = npp_hi),
              fill = NA,
              linetype = "dashed",
              color = "black",
              inherit.aes = FALSE) +
    scale_color_manual(values = model_colors) +
    coord_cartesian(xlim = c(0, 9), ylim = c(0, 11)) +
    labs(x = "LAI", y = expression(NPP ~ (MgC ~ ha^-1 ~ year ^ -1))) +
    ## guides(color = guide_legend(nrow = 4)) +
    guides(color = FALSE) +
    facet_wrap(vars(model), scales = "fixed",
               labeller = my_labeller) +
    theme_cowplot() +
    theme(legend.position = "bottom",
          strip.background = element_blank())
)

if ("--poster" %in% cmdargs) source("analysis/drake_poster.R")

# Parallelism configuration. Not sure which of these is better...
future::plan(future.callr::callr)

dconf <- drake_config(
  plan,
  parallelism = "future",
  jobs = availableCores()
)

if ("make" %in% cmdargs) {
  message("Running `drake::make`")
  make(config = dconf)
} else if (!interactive()) {
  dconf
}
