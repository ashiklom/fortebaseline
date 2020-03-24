### Pairs plots
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
