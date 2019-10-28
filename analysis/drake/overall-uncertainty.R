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
  observations = tribble(
    ~variable, ~low, ~mean, ~hi,
    "npp", 6, 6.5, 7,
    "lai", 3.97 - 1.96 * 0.423, 3.97, 3.97 + 1.96 * 0.423
  ) %>%
    expand_grid(model = models$model),
  ts_both = bind_rows(scalar_data, pft_totals) %>%
    mutate(
      model_id = substr(case, 4, 6),
      variable = stringr::str_remove(variable, "^mmean_") %>%
        stringr::str_remove("_py$")
    ) %>%
    filter(variable %in% c("npp", "lai")),
  ts_summary = ts_both %>%
    group_by(model_id, year, variable) %>%
    summarize(
      hi = quantile(value, 0.9),
      lo = quantile(value, 0.1)
    ) %>%
    left_join(models, "model_id"),
  summary_ts_plot_gg = ts_both %>%
    left_join(models, "model_id") %>%
    ggplot() +
    aes(x = year, color = color) +
    geom_line(aes(y = value, group = case), alpha = 0.1, size = 0.3) +
    ## # ED2 default results.
    ## geom_line(
    ##   aes(y = value),
    ##   data = rename(structure_default_data, variable = name),
    ##   linetype = "solid", color = "black"
    ## ) +
    geom_ribbon(
      aes(ymin = lo, ymax = hi),
      data = ts_summary,
      color = "black",
      fill = NA,
      linetype = "dashed"
    ) +
    geom_pointrange(
      aes(x = 2000, y = mean, ymin = low, ymax = hi),
      data = observations,
      color = "black",
      inherit.aes = FALSE
    ) +
    facet_grid(
      vars(variable),
      vars(fct_relabel(model, ~gsub(" ", "\n", .))),
      scales = "free_y",
      switch = "y",
      labeller = labeller(
        variable = as_labeller(c(
          "npp" = "NPP ~ (MgC ~ ha^{-1} ~ year^{-1})",
          "lai" = "LAI"
        ), default = label_parsed)
      )
    ) +
    scale_color_identity() +
    theme_cowplot() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90),
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