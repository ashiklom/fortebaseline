## plan <- bind_plans(plan, drake_plan(

## ))

stop()

wcplot_data = both_wide %>%
  filter(year == 1920) %>%
  select(param_id:traits, `Early hardwood`:Pine) %>%
  left_join(select(params_wide, param_id, ends_with("water_conductance")),
            "param_id") %>%
  ## mutate_at(vars(ends_with("water_conductance")), log10) %>%
  mutate(npft_eff = 1 / (`Early hardwood`^2 + `Mid hardwood`^2 + `Late hardwood`^2 + Pine^2))

wcplot_data %>%
  arrange(npft_eff) %>%
  ## mutate(npft_eff = na_if(npft_eff, 1)) %>%
  ggplot() +
  aes(x = Early.water_conductance, y = Mid.water_conductance, color = npft_eff,
      size = npft_eff) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(model), scales = "fixed") +
  scale_color_viridis_c(na.value = "gray80")

wcplot_data_2 <- wcplot_data %>%
  pivot_longer(`Early hardwood`:Pine, names_to = "pft", values_to = "frac") %>%
  pivot_longer(ends_with("water_conductance"), names_to = "trait_name", values_to = "trait_value") %>%
  separate(trait_name, c("trait_pft", "trait_name"), sep = "\\.") %>%
  mutate(pft_short = factor(pft, pfts("pft"), pfts("shortname"))) %>%
  filter(trait_pft == pft_short)

ggplot(wcplot_data_2) +
  aes(x = trait_value, y = frac, color = pft_short) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_smooth(se = FALSE) +
  scale_color_viridis_d() +
  facet_wrap(vars(model), scales = "fixed")

wcplot_data_long = wcplot_data %>%
  select(model, npft_eff, ends_with("water_conductance")) %>%
  pivot_longer(ends_with("water_conductance")) %>%
  mutate(name = gsub("\\.water_conductance", "", name),
         npft_eff_cut = cut(npft_eff, c(-Inf, 1.1, 2, 3, Inf)))

wcplot_data_long %>%
  filter_at(vars(model, npft_eff), negate(is.na)) %>%
  ggplot() +
  aes(x = npft_eff_cut, y = value, color = name) +
  geom_boxplot() +
  ## geom_jitter(size = 0.2, alpha = 0.4) +
  scale_y_log10() +
  facet_wrap(vars(model), scales = "fixed",
             nrow = 4)

wcplot_data %>%
  filter(!is.na(model), !is.na(npft_eff)) %>%
  mutate(npft_eff_cut = cut(npft_eff, c(-Inf, 1.1, 2, 3, Inf))) %>%
  ggplot(aes(x = npft_eff_cut, y = Mid.water_conductance)) +
  geom_violin() +
  geom_jitter(size = 0.4, alpha = 0.4) +
  facet_wrap(vars(model), scales = "fixed") +
  scale_y_log10()

wcplot_data %>%
  filter(npft_eff > 1) %>%
  plotly::plot_ly(
    x = ~Early.water_conductance,
    y = ~Mid.water_conductance,
    z = ~Late.water_conductance,
    color = ~npft_eff
  )


params_wide

both_wide %>%
  filter(year == 1980) %>%
