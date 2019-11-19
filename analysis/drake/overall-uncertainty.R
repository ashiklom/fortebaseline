cohort_file <- path(download_dir, "all-output-monthly-cohort.fst")
pft_file <- path(download_dir, "all-output-monthly-pft.fst")
scalar_file <- path(download_dir, "all-output-monthly-scalar.fst")
soil_file <- path(download_dir, "all-output-monthly-soil.fst")

### Read files
plan <- plan <- bind_plans(plan, drake_plan(
  scalar_vars = sprintf("mmean_%s_py", c("gpp", "plresp", "npp")),
  scalar_cols = c("case", "datetime", scalar_vars),
  scalar_data = setDT(fst(file_in(!!scalar_file))[, scalar_cols]) %>%
    .[, lapply(.SD, mean), .(case, year = year(datetime))] %>%
    # Unit conversion: kg m-2 -> Mg ha-1
    .[, `:=`(
      mmean_gpp_py = mmean_gpp_py * 10,
      mmean_plresp_py = mmean_plresp_py * 10,
      mmean_npp_py = mmean_npp_py * 10
    )] %>%
    melt(id.vars = c("case", "year")) %>%
    as_tibble(),
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
  observations =
    tribble(
    ~variable, ~low, ~mean, ~hi,
    "npp", 6, 6.5, 7,
    "lai", 3.97 - 1.96 * 0.423, 3.97, 3.97 + 1.96 * 0.423,
    "prod_eff", 6 / (3.97 - 1.96 * 0.423), 6.5 / 3.97, 7 / (3.97 + 1.96 * 0.423)
  ) %>%
    expand_grid(model = models$model),
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
      hi = quantile(value, 0.9, na.rm = TRUE),
      lo = quantile(value, 0.1, na.rm = TRUE)
    ) %>%
    left_join(models, "model_id"),
  obs_plot = observations %>%
    mutate(
      variable = factor(variable, c("npp", "lai", "prod_eff", "d2"), c(
        "atop(NPP, (MgC ~ ha^{-1} ~ year^{-1}))",
        "LAI",
        "'NPP / LAI'",
        "N['PFT,eff']"
      ))
    ),
  summary_ts_plot_gg = ts_both2 %>%
    left_join(models, "model_id") %>%
    filter(variable %in% c("npp", "lai", "prod_eff", "d2")) %>%
    mutate(
      variable = factor(variable, c("npp", "lai", "prod_eff", "d2"), c(
        "atop(NPP, (MgC ~ ha^{-1} ~ year^{-1}))",
        "LAI",
        "'NPP / LAI'",
        "N['PFT,eff']"
      ))
    ) %>%
    ggplot() +
    aes(x = year, color = color) +
    geom_line(aes(y = value, group = case), alpha = 0.1, size = 0.3) +
    geom_pointrange(
      aes(x = 2000, y = mean, ymin = low, ymax = hi),
      data = obs_plot,
      color = "black",
      inherit.aes = FALSE
    ) +
    facet_grid(
      vars(variable),
      vars(fct_relabel(model, ~gsub(" ", "\n", .))),
      scales = "free_y",
      switch = "y",
      labeller = label_parsed
    ) +
    scale_color_identity() +
    theme_cowplot() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.background = element_blank(),
      strip.placement = "outside"
    ),
  summary_ts_plot_png = ggsave(
    file_out("analysis/figures/summary-ts-plot.png"),
    summary_ts_plot_gg,
    width = 10, height = 6
  ),
  default_ts_plot_knit = knitr::include_graphics(file_in(
    "analysis/figures/summary-ts-plot.png"
  ))
))

### STOP HERE
stop()
### Download files
cohort_osf <- "..."
pft_osf <- "..."
scalar_osf <- "..."
soil_osf <- "..."

plan <- bind_plans(plan, drake_plan(
  cohort_file_dl = target(
    download.file(osf_url(cohort_osf), file_out(!!cohort_file)),
    trigger = trigger(change = get_timestamp(cohort_osf),
                      condition = !file.exists(cohort_file))
  ),
))
