### Median results for sensitivity analysis
plan <- bind_plans(plan, drake_plan(
  median_fluxes = median_results %>%
    select(casename, scalar) %>%
    unnest(scalar) %>%
    select(case, param_id, datetime, mmean_npp_py) %>%
    group_by(case, param_id, year = year(datetime)) %>%
    summarize(mmean_npp_py = mean(mmean_npp_py)) %>%
    ungroup() %>%
    mutate(model_id = stringr::str_sub(case, -3, -1)),
  median_pft = median_results %>%
    select(casename, pft_py) %>%
    unnest(pft_py) %>%
    select(case, param_id, datetime, pft, agb_py, mmean_lai_py) %>%
    mutate(
      pft = set_pft(pft),
      model_id = stringr::str_sub(case, -3, -1)
    ) %>%
    filter(month(datetime) >= 6, month(datetime) <= 8) %>%
    group_by(case, param_id, year = year(datetime), pft) %>%
    select(-datetime) %>%
    summarize_all(mean) %>%
    mutate(frac_agb = agb_py / sum(agb_py)) %>%
    ungroup(),
  median_lai = median_pft %>%
    group_by(case, param_id, year) %>%
    summarize(mmean_lai_py = sum(mmean_lai_py)) %>%
    ungroup(),
  median_pft_frac = median_pft %>%
    select(case, param_id, year, pft, frac_agb) %>%
    mutate(pft = factor(pft, levels(pft), c("EH", "MH", "LH", "Pine"))) %>%
    setDT() %>%
    dcast(case + param_id + year ~ pft, value.var = "frac_agb") %>%
    as_tibble(),
  median_params = median_results[["trait_values"]][[1]] %>%
    bind_rows(.id = "pft") %>%
    mutate(pft = factor(pft, pfts("bety_name"), pfts("shortname"))) %>%
    pivot_longer(-pft) %>%
    unite(variable, c(pft, name), sep = ".") %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(param_id = -1),
  median_wide = median_fluxes %>%
    left_join(median_lai, c("case", "param_id", "year")) %>%
    left_join(median_pft_frac, c("case", "param_id", "year")) %>%
    left_join(models, "model_id") %>%
    mutate(param_id = -1) %>%
    left_join(median_params, "param_id")
))

### Inputs for sensitivity analysis
do_sensitivity <- function(y, x, ymedian, xmedian) {
  tryCatch(
    list(sensitivity_analysis(y, x, ymedian[1], xmedian[1])),
    error = function(e) NULL
  )
}

plan <- bind_plans(plan, drake_plan(
  sensitivity_vars = c("mmean_npp_py", "mmean_lai_py", "EH", "MH", "LH", "Pine"),
  sensitivity_groups = bind_rows(
    tibble(year = 1902:1999, sgroup = "1902-1999"),
    tibble(year = 1920:1950, sgroup = "1920-1950"),
    tibble(year = 1975:1999, sgroup = "1975-1999")
  ),
  params_wide = params %>%
    rename(pft = name) %>%
    mutate(pft = factor(pft, pfts("bety_name"), pfts("shortname"))) %>%
    pivot_longer(-c(param_id, pft)) %>%
    unite(variable, c(pft, name), sep = ".") %>%
    pivot_wider(names_from = "variable", values_from = "value"),
  sensitivity_inputs = target(
      both_wide %>%
        inner_join(sensitivity_groups, "year") %>%
        rename(
          EH = `Early hardwood`,
          MH = `Mid hardwood`,
          LH = `Late hardwood`
        ) %>%
        select(case, model_id, param_id, sgroup, all_of(sensitivity_vars)) %>%
        group_by(case, model_id, param_id, sgroup) %>%
        summarize_all(mean, na.rm = TRUE) %>%
        ungroup() %>%
        pivot_longer(
          all_of(sensitivity_vars),
          names_to = "yvar",
          values_to = "y"
        ) %>%
        left_join(params_wide, "param_id") %>%
        pivot_longer(-c(case, model_id, param_id, sgroup, yvar, y),
                     names_to = "xvar", values_to = "x"),
      format = "fst_tbl"
    ),
  median_sens_inputs = median_wide %>%
    inner_join(sensitivity_groups, "year") %>%
    select(model_id, sgroup, all_of(sensitivity_vars),
           matches(paste(pfts("shortname"), collapse = "|"))) %>%
    group_by(model_id, sgroup) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(
      all_of(sensitivity_vars),
      names_to = "yvar",
      values_to = "ymedian"
    ) %>%
    pivot_longer(
      -c(model_id, sgroup, yvar, ymedian),
      names_to = "xvar",
      values_to = "xmedian"
    ),
  combined_sens_inputs = target(
    sensitivity_inputs %>%
      left_join(median_sens_inputs, c("model_id", "sgroup", "yvar", "xvar")),
    format = "fst_tbl"
  ),
  sensitivity_raw_results = combined_sens_inputs %>%
    group_by(model_id, sgroup, yvar, xvar) %>%
    summarize(result = list(do_sensitivity(y, x, ymedian, xmedian)))
))

### Sensitivity analysis outputs
sensplot <- function(dat, sgroup, metric, yvar) {
  metric <- rlang::enquo(metric)
  dat %>%
    filter(sgroup == !!sgroup) %>%
    top_n_sensitivity_plot(yvar, !!metric, 8, "free") +
    facet_wrap(vars(model), scales = "free_y",
               drop = TRUE, ncol = 2, dir = "v") +
    scale_color_manual(values = pfts("color")) +
    cowplot::theme_cowplot() +
    theme(
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 12)
    )
}

plan <- bind_plans(plan, drake_plan(
  sensitivity_proc = sensitivity_raw_results %>%
    unnest(result) %>%
    unnest(result) %>%
    ungroup(),
  sensitivity_sub = sensitivity_proc %>%
    filter(
      grepl("mmean_", yvar),
      is.finite(sensitivity),
      is.finite(elasticity)
    ) %>%
    group_by(model_id, yvar) %>%
    mutate(fpvar = pvar / sum(pvar)) %>%
    group_by(xvar) %>%
    mutate(total_pvar = sum(pvar)) %>%
    ungroup() %>%
    separate(xvar, c("shortname", "xvar"), extra = "merge") %>%
    mutate(
      shortname = factor(shortname, pfts("shortname"), pfts("pft")),
      model = factor(model_id, levels(models$model_id), levels(models$model)),
      xvar = factor(xvar, ed2_param_table[["ED Name"]],
                    ed2_param_table[["Display name"]])
    ),
  sensitivity_periodall_gg =
    sensplot(sensitivity_sub, "1902-1999", fpvar, "mmean_npp_py") +
    labs(y = "Partial variance", color = "Plant functional type") +
    guides(color = guide_legend(
      nrow = 2, byrow = TRUE,
      override.aes = list(size = 4, linetype = 0)
    )) +
    theme(strip.background = element_blank(),
          legend.position = "bottom"),
  sensitivity_periodall_png = cowplot::ggsave2(file_out(!!here(
    "analysis", "figures", "partial-variance-npp-all.png"
  )), sensitivity_periodall_gg, width = 7.6, height = 8.4),
  sensitivity_period1_gg =
    sensplot(sensitivity_sub, "1920-1950", fpvar, "mmean_npp_py") +
    labs(y = "Partial variance", color = "Plant functional type") +
    guides(color = guide_legend(
      nrow = 2, byrow = TRUE,
      override.aes = list(size = 4, linetype = 0)
    )) +
    theme(strip.background = element_blank(),
          legend.position = "bottom"),
  sensitivity_period1_png = cowplot::ggsave2(file_out(!!here(
    "analysis", "figures", "partial-variance-npp-peak.png"
  )), sensitivity_period1_gg, width = 7.6, height = 8.4),
  sensitivity_period2_gg =
    sensplot(sensitivity_sub, "1975-1999", fpvar, "mmean_npp_py") +
    labs(y = "Partial variance", color = "Plant functional type") +
    guides(color = guide_legend(
      nrow = 2, byrow = TRUE,
      override.aes = list(size = 4, linetype = 0)
    )) +
    theme(strip.background = element_blank(),
          legend.position = "bottom"),
  sensitivity_period2_png = cowplot::ggsave2(file_out(!!here(
    "analysis", "figures", "partial-variance-npp-end.png"
  )), sensitivity_period2_gg, width = 7.6, height = 8.4)
))
