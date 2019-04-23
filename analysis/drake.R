#!/usr/bin/env Rscript
library(drake)
library(ggplot2)
## library(fortebaseline)
devtools::load_all(here::here(), attach_testthat = FALSE)
expose_imports("fortebaseline")

# begin imports
import::from("dplyr", "tbl", "filter", "select", "collect", "mutate",
             "pull", "case_when", "rename", "ungroup", "group_by", "left_join",
             "if_else", "group_by_at", "bind_rows", "summarize_all", "summarize",
             "arrange", "distinct", "one_of", "everything", "summarize_at",
             "starts_with", "row_number", "transmute", .into = "")
import::from("tidyr", "unnest", "spread", .into = "")
import::from("tibble", "as_tibble", "tribble", .into = "")
import::from("here", "here", .into = "")
import::from("fs", "dir_create", "path", .into = "")
import::from("fst", "write_fst", "read_fst", .into = "")
import::from("forcats", "fct_relabel", "lvls_revalue", "fct_inorder",
             .into = "")
import::from("cowplot", "save_plot", "theme_cowplot", .into = "")
import::from("furrr", "future_pmap_dfr", .into = "")
import::from("magrittr", "%>%", .into = "")
import::from("purrr", "map", "possibly", "discard", "safely",
             "negate", "map_dfr", .into = "")
import::from("future.callr", "callr", .into = "")
import::from("future", "plan", "availableCores", .into = "")
import::from("lubridate", "floor_date", "month", "%m+%", "period", 
    .into = "")
# end imports

created_since <- "2019-03-20"

workflow_df <- tryCatch(
  readd(workflow_df_drake),
  error = function(e) {
    tbl(default_connection(), "workflows") %>%
      filter(start_date == "1902-06-01",
             end_date == "1912-12-31",
             created_at > created_since) %>%
      select(workflow_id = id, start_date, end_date, notes) %>%
      collect() %>%
      mutate(configs_df = map(notes, parse_notes),
             workflow_id = as.numeric(workflow_id)) %>%
      unnest(configs_df) %>%
      select(-notes)
  }
)

workflows_years <- tryCatch(
  readd(workflows_years_drake),
  error = function(e) {
    expand.grid(
      workflow_id = as.numeric(pull(workflow_df, workflow_id)),
      year = seq(1902, 1912)
    ) %>%
      as_tibble() %>%
      mutate(startmonth = case_when(year == 1902 ~ 6, TRUE ~ 1))
  }
)

plan <- drake_plan(
  #######################################
  ## Figure 1: Parameter distributions ##
  #######################################
  ma_posterior = readRDS(file_in(
    !!here::here("analysis", "data", "derived-data", "meta-analysis.rds")
  )) %>% tidy_posterior(),
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
  raw_monthly_output = read_fst(file_in(
    !!here::here("analysis", "data", "derived-data", "monthly-ensemble-output.fst")
  )) %>%
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
    select(starts_with("mmean"), agb_py) %>%
    # Remove duplicated values -- `mmean` values should be unique by PFT.
    summarize_all(mean),
  monthly_means_site = monthly_means %>%
    select(
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
    scale_color_brewer(palette = "Paired") +
    scale_fill_brewer(palette = "Paired") +
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
  run_params_all = read_fst(file_in(
    !!here("analysis", "data", "derived-data", "ed-ensemble-params.fst")
  )) %>% as_tibble(),
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
  #####################
  ## Old drake stuff ##
  #####################
  workflow_df_drake = workflow_df,
  workflows_years_drake = workflows_years,
  lai_raw = target(
    possibly(get_monthly_lai, NULL)(workflow_id, year, startmonth),
    transform = map(!!!workflows_years),
    # HACK: Don't re-download all of these. It takes forever, and
    # isn't very reliable.
    trigger = trigger(condition = FALSE, mode = "condition")
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
                                 "plastic traits", "static traits") %>%
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
      knitr_in(!!(here("analysis", "paper", "paper.Rmd"))),
      "github_document"
    )),
  # Representative workflows for two-stream and multiple scatter
  wfid_twostream = workflow_df %>%
    filter(!multiple_scatter, !crown_model, !trait_plasticity,
           !n_limit_soil, n_limit_ps) %>%
    pull(workflow_id),
  wfid_multiscatter = workflow_df %>%
    filter(multiple_scatter, !crown_model, !trait_plasticity,
           !n_limit_soil, n_limit_ps) %>%
    pull(workflow_id),
  rad_histfile = target(
    download_history_file(wid, 1910, 7, day, hour),
    transform = cross(wid = c(wfid_twostream, wfid_multiscatter),
                      day = !!(seq(1, 10)),
                      hour = !!(seq(0, 18, 6)))
  ),
  rad_results_tidy = target(
    read_tidy_rad_profile(rad_histfile),
    transform = map(rad_histfile)
  ),
  rad_results = target(
    bind_rows(rad_results_tidy) %>%
      mutate(rtm = case_when(workflow_id == wfid_twostream ~ "two stream",
                             workflow_id == wfid_multiscatter ~ "multiple scatter") %>%
               factor(c("two stream", "multiple scatter"))),
    transform = combine(rad_results_tidy)
  ),
  rad_results_long = rad_results %>%
    filter(lubridate::hour(datetime) == 18) %>%
    group_by(workflow_id, rtm, cohort) %>%
    select(-datetime) %>%
    summarize_all(mean) %>%
    tidyr::gather(variable, value, -workflow_id, -rtm, -cohort) %>%
    tidyr::separate(variable, c("wave", "type", "direction")),
  rad_lineplot = rad_results_long %>%
    filter(value > 0) %>%
    ggplot() +
    aes(x = cohort, y = value, linetype = rtm) +
    geom_line() +
    facet_grid(vars(wave, type), vars(direction), scales = "free") +
    labs(x = "Cohort (tallest -> shortest)",
         y = expression("Radiation" ~ (W ~ m ^ {-2})),
         linetype = "RTM type") +
    theme_cowplot() +
    theme(legend.position = "bottom"),
  rad_barplot = rad_results_long %>%
    filter(cohort == 1) %>%
    ggplot() +
    aes(x = rtm, y = value) +
    geom_col() +
    facet_grid(vars(wave, type), vars(direction), scales = "free") +
    labs(x = "RTM type",
         y = expression("Radiation" ~ (W ~ m ^ {-2}))) +
    theme_cowplot(),
  # Trait plasticity analysis
  plastic_wfids = workflow_df %>%
    filter(trait_plasticity) %>%
    pull(workflow_id),
  cohort_raw_data = target(
    map(plastic_wfids, safely(read_cohort_history),
        year = years, month = months),
    transform = cross(years = !!seq(1902, 1910),
                      months = !!seq(7, 8, 9))
  ),
  cohort_raw_list = target(
    c(cohort_raw_data),
    transform = combine(cohort_raw_data)
  ),
  cohort_data = cohort_raw_list %>%
    discard(~is.null(.x[["result"]])) %>%
    map_dfr("result"),
  # Old parameter sensitivity analysis
  old_lai_summary = file_in(!!file.path("analysis", "data",
                                        "derived-data", "ed-lai-output.fst")) %>%
    read_fst() %>%
    as_tibble() %>%
    group_by(year = floor_date(month, "year"),
             pft, run_id = ensemble) %>%
    summarize(max_lai = max(lai)) %>%
    ungroup(),
  old_lai_plot = old_lai_summary %>%
    filter(pft != "Late conifer") %>%
    ggplot() +
    aes(x = year, y = max_lai, color = pft) +
    geom_line() +
    ## facet_wrap(vars(factor(run_id, old_npp_max[["run_id"]]))) +
    facet_wrap(vars(factor(run_id))) +
    labs(y = "Max annual leaf area index",
         color = "PFT") +
    theme_cowplot() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom"),
  ## old_ensemble = file_in(!!file.path("analysis", "data",
  ##                                    "derived-data", "ed-ensemble-out.fst")) %>%
  ##   read_fst() %>%
  ##   as_tibble() %>%
  ##   spread(variable, value) %>%
  ##   group_by(run_id, year = floor_date(time, "year")) %>%
  ##   summarize(
  ##     npp = udunits2::ud.convert(sum(npp), "kg m-2", "Mg ha-1") * 60 * 30,
  ##     gpp = udunits2::ud.convert(sum(gpp), "kg m-2", "Mg ha-1") * 60 * 30,
  ##     lai = max(lai)
  ##   ) %>%
  ##   ungroup() %>%
  ##   tidyr::gather(variable, value, npp, gpp, lai),
  ## old_npp_max = old_ensemble %>%
  ##   filter(year > "1959-12-31", variable == "npp") %>%
  ##   group_by(run_id) %>%
  ##   summarize(npp = mean(value)) %>%
  ##   ungroup() %>%
  ##   arrange(npp),
  ## hardiman = tribble(
  ##   ~variable, ~low, ~mean, ~hi,
  ##   "LAI", 1.8, 4.14, 6.56,
  ##   "NPP", 1.68, 3.11, 7.26
  ## ) %>% mutate(variable = factor(variable, c("GPP", "NPP", "LAI"))),
  ## old_ensemble_plot = old_ensemble %>%
  ##   mutate(variable = factor(variable, c("gpp", "npp", "lai")) %>%
  ##            lvls_revalue(c("GPP", "NPP", "LAI"))) %>%
  ##   ggplot() +
  ##   aes(x = year, y = value, group = run_id) +
  ##   geom_line(alpha = 0.2) +
  ##   geom_hline(aes(yintercept = low), data = hardiman,
  ##              color = "red", linetype = "dashed") +
  ##   geom_hline(aes(yintercept = mean), data = hardiman,
  ##              color = "red") +
  ##   geom_hline(aes(yintercept = hi), data = hardiman,
  ##              color = "red", linetype = "dashed") +
  ##   facet_grid(
  ##     rows = vars(variable),
  ##     scales = "free_y",
  ##     labeller = labeller(
  ##       variable = as_labeller(c(
  ##         GPP = "GPP ~ (Mg ~ ha^{-1})",
  ##         NPP = "NPP ~ (Mg ~ ha^{-1})",
  ##         LAI = "LAI"
  ##       ), default = label_parsed)
  ##     )
  ##   ) +
  ##   theme_cowplot() +
  ##   theme(axis.title = element_blank()),
)

# Parallelism configuration. Not sure which of these is better...
future::plan(future.callr::callr)
## future::plan(future::multiprocess)

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

# It's not recommended to run `drake::make` interactively, but it's
# convenient. This is a workaround.
called_with_callr <- as.logical(nchar(Sys.getenv("CALLR")))
if (interactive()) {
  # Use r_make to build this reproducibly
  r_make(here("analysis/drake.R"),
         callr::r,
         r_args = list(env = c(callr::rcmd_safe_env(), "CALLR" = "TRUE")))
} else if (called_with_callr) {
  # Called with callr::r -- return `dconf`
  message("Called with callr -- returning dconf")
  dconf
} else {
  # Probably called from a script 
  message("Called from script. Running `drake::make`.")
  make(config = dconf)
}
