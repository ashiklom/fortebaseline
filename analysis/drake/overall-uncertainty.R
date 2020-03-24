plan <- bind_plans(plan, drake_plan(
  cohort_file_dl = target(
    download.file(osf_url(cohort_osf), file_out(!!cohort_file)),
    trigger = trigger(change = get_timestamp(cohort_osf),
                      condition = !file.exists(cohort_file)),
    hpc = FALSE
  ),
  pft_file_dl = target(
    download.file(osf_url(pft_osf), file_out(!!pft_file)),
    trigger = trigger(change = get_timestamp(pft_osf),
                      condition = !file.exists(pft_file)),
    hpc =  FALSE
  ),
  scalar_file_dl = target(
    download.file(osf_url(scalar_osf), file_out(!!scalar_file)),
    trigger = trigger(change = get_timestamp(scalar_osf),
                      condition = !file.exists(scalar_file)),
    hpc = FALSE
  ),
  soil_file_dl = target(
    download.file(osf_url(soil_osf), file_out(!!soil_file)),
    trigger = trigger(change = get_timestamp(soil_osf),
                      condition = !file.exists(soil_file)),
    hpc = FALSE
  )
))

### Read files
get_annual_means <- function(file, vars) {
  .datatable.aware <- TRUE                #nolint
  all_cols <- c("case", "datetime", vars)
  dat <- data.table::setDT(fst::fst(file)[, all_cols])
  dat_avg <- dat[, lapply(.SD, mean), .(case, year = lubridate::year(datetime))]
  for (j in vars) {
    data.table::set(dat_avg, j = j, value = dat_avg[, ..j] * 10)
  }
  dat_long <- data.table::melt(dat_avg, id.vars = c("case", "year"))
  tibble::as_tibble(dat_long)
}

plan <- bind_plans(plan, drake_plan(
  scalar_variables = sprintf("mmean_%s_py", c("gpp", "plresp", "npp")),
  scalar_data = get_annual_means(file_in(!!scalar_file), scalar_variables),
  pft_cols = c("case", "datetime", "pft", "agb_py", "mmean_lai_py"),
  pft_data = setDT(fst(file_in(!!pft_file))[, pft_cols]) %>%
    .[between(month(datetime), 6, 8), lapply(.SD, mean),
      .(case, pft, year = year(datetime))] %>%
    .[, pft := set_pft(pft)] %>%
    .[, agb_frac := agb_py / sum(agb_py), .(case, year)] %>%
    as_tibble(),
  pft_totals = pft_data %>%
    group_by(case, year) %>%
    summarize_at(vars(-pft, -agb_frac), sum) %>%
    pivot_longer(-c(case, year), names_to = "variable", values_to = "value")
))

### Summary plot
plan <- bind_plans(plan, drake_plan(
  # Observed NPP from Gough et al. 2008 Ag For Met.
  obs_npp = list(lo = 6.07, mean = 6.54, hi = 7.01),
  # Observed LAI from Ameriflux BADM files for US_UMB
  obs_lai = list(mean = 3.97, sd = 0.423) %>%
    modifyList(., list(lo = .$mean - 1.96 * .$sd,
                       hi = .$mean + 1.96 * .$sd)) %>%
    .[c("lo", "mean", "hi")],
  observations = expand_grid(
    tribble(
      ~variable, ~low, ~mean, ~hi,
      "npp", obs_npp$lo, obs_npp$mean, obs_npp$hi,
      "lai", obs_lai$lo, obs_lai$mean, obs_lai$hi,
      "prod_eff", obs_npp$lo / obs_lai$hi, obs_npp$mean / obs_lai$mean, obs_npp$hi / obs_lai$lo  #nolint
    ),
    model = models[["model"]]
  ),
  ts_both = bind_rows(scalar_data, pft_totals) %>%
    mutate(
      model_id = substr(case, 4, 6),
      variable = stringr::str_remove(variable, "^mmean_") %>%
        stringr::str_remove("_py$")
    ) %>%
    filter(variable %in% c("npp", "lai")),
  ts_both2 = ts_both %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(prod_eff = npp / lai) %>%
    pivot_longer(c(npp, lai, prod_eff), names_to = "variable", values_to = "value") %>%
    filter(!(variable == "prod_eff" & (abs(value) > 40 | value < -5))) %>%
    bind_rows(pft_n_effective %>% mutate(model_id = substr(case, 4, 6))),
  ts_summary = ts_both2 %>%
    group_by(model_id, year, variable) %>%
    summarize(
      max = max(value, na.rm = TRUE),
      hi2 = quantile(value, 0.95, na.rm = TRUE),
      hi1 = quantile(value, 0.75, na.rm = TRUE),
      lo1 = quantile(value, 0.25, na.rm = TRUE),
      lo2 = quantile(value, 0.05, na.rm = TRUE),
      min = min(value, na.rm = TRUE)
    ) %>%
    left_join(models, "model_id") %>%
    filter(variable %in% c("npp", "lai", "prod_eff", "d2")),
  summary_ts_data = ts_both2 %>%
    left_join(models, "model_id") %>%
    filter(variable %in% c("npp", "lai", "prod_eff", "d2")),
  summary_ts_plot_gg = ggplot(ts_summary) +
    aes(x = year) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = "gray90") +
    geom_ribbon(aes(ymin = lo2, ymax = hi2), fill = "gray70") +
    geom_ribbon(aes(ymin = lo1, ymax = hi1), fill = "gray40") +
    geom_line(
      aes(y = value, group = case, color = label),
      data = ts_params
    ) +
    geom_pointrange(
      aes(x = 2000, y = mean, ymin = low, ymax = hi),
      data = observations,
      color = "black",
      inherit.aes = FALSE
    ) +
    facet_grid(
      vars(
        variable = factor(variable, c("npp", "lai", "prod_eff", "d2"), c(
          "atop(NPP, (MgC ~ ha^{-1} ~ year^{-1}))",
          "LAI",
          "'NPP / LAI'",
          "N['PFT,eff']"
        ))),
      vars(model = fct_relabel(model, ~gsub(" ", "\n", .))),
      scales = "free_y",
      switch = "y",
      labeller = labeller(variable = label_parsed, model = label_value)
    ) +
    scale_color_brewer(palette = "Set1") +
    labs(color = "Param. set") +
    theme_cowplot() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(size = rel(0.78))
    ),
  summary_ts_plot_png = ggsave(
    file_out("analysis/figures/summary-ts-plot.png"),
    summary_ts_plot_gg,
    width = 10, height = 6
  )
))

plan <- bind_plans(plan, drake_plan(
  ts_params = summary_ts_data %>%
    mutate(param_id = as.numeric(substr(case, 0, 3))) %>%
    inner_join(use_params, "param_id") %>%
    mutate(label = fct_rev(label))
))
