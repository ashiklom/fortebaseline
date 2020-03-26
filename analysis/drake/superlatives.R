### Superlatives
plan <- bind_plans(plan, drake_plan(
  pft_wide = pft_data %>%
    mutate(model_id = substr(case, 4, 6)) %>%
    select(case, pft, year, model_id, agb_frac) %>%
    setDT() %>%
    dcast(case + year + model_id ~ pft, value.var = "agb_frac") %>%
    as_tibble(),
  lai_wide = pft_totals %>%
    filter(variable == "mmean_lai_py") %>%
    select(case, year, mmean_lai_py = value),
  both_wide = scalar_data %>%
    setDT() %>%
    dcast(case + year ~ variable, value.var = "value") %>%
    as_tibble() %>%
    left_join(pft_wide, c("case", "year")) %>%
    left_join(lai_wide, c("case", "year")) %>%
    mutate(model_id = substr(case, 4, 6),
           param_id = as.numeric(substr(case, 0, 3))) %>%
    left_join(models, c("model_id")) %>%
    mutate(
      npft_eff = 1 / (`Early hardwood`^2 + `Mid hardwood`^2 +
                        `Late hardwood`^2 + Pine^2)
    )
))

plan <- bind_plans(plan, drake_plan(
  # Average of last 10 simulation years
  last_ten = both_wide %>%
    filter(year > 1990) %>%
    group_by_at(vars(param_id:traits)) %>%
    summarize_at(vars(ends_with("_py"), ends_with("hardwood"), Pine, npft_eff), mean) %>%
    ungroup(),
  fit_observed = last_ten %>%
    filter(
      mmean_npp_py >= obs_npp$lo, mmean_npp_py <= obs_npp$hi,
      mmean_lai_py >= obs_lai$lo, mmean_lai_py <= obs_lai$hi
    ) %>%
    mutate(category = "Fit observations") %>%
    select(param_id, model, category),
  high_diversity = last_ten %>%
    ## arrange(desc(npft_eff)) %>%
    ## top_n(10) %>%
    filter(npft_eff > 2) %>%
    mutate(category = "Most diverse") %>%
    select(param_id, model, category),
  superlatives = bind_rows(fit_observed, high_diversity)
))

plan <- bind_plans(plan, drake_plan(
  ## drop_params = c("SLA", "Root:Leaf"),
  drop_params = c(),
  params_wide_fit_observed = params_wide %>%
    mutate_at(vars(ends_with("water_conductance")), log10) %>%
    inner_join(fit_observed_params, "param_id"),
  params_fit_observed = params_wide_fit_observed %>%
    pivot_longer(
      -c(param_id, label),
      names_to = "variable",
      values_to = "value"
    ) %>%
    extract(variable, c("PFT", "trait"), "(.*?)\\.(.*)") %>%
    mutate(shortname = factor(PFT, pfts("shortname"))) %>%
    left_join(ed2_param_table, c("trait" = "ED Name")) %>%
    mutate(
      `Display name` = fct_recode(`Display name`,
                                  "log10(Water cond.)" = "Water cond.")
    ) %>%
    filter(!`Display name` %in% drop_params),
  params_fit_observed_gg = param_dist_data %>%
    filter(!`Display name` %in% drop_params) %>%
    ggplot() +
    aes(x = shortname, y = draws) +
    geom_violin(alpha = 0.3, fill = "gray80") +
    geom_point(aes(y = value, color = label),
               position = position_dodge(width = 0.3),
               data = params_fit_observed) +
    # Parameter values
    facet_wrap(vars(`Display name`), scales = "free_y") +
    labs(x = "PFT", color = "Param. set") +
    scale_color_brewer(palette = "Set1") +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "right",
          legend.box = "vertical"),
  params_fit_observed_png = ggsave(
    file_out("analysis/figures/params-fit-observed.png"),
    params_fit_observed_gg,
    width = 15.3, height = 9.9, dpi = 300
  )
))
