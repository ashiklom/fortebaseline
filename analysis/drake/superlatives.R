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
    left_join(
      cases %>%
        select(case, model_id, param_id),
      c("case", "model_id")
    ) %>%
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
    arrange(desc(npft_eff)) %>%
    top_n(10) %>%
    mutate(category = "Most diverse") %>%
    select(param_id, model, category),
  superlatives = bind_rows(fit_observed, high_diversity)
))
