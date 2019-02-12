library(tidyverse)
library(fst)

ed_in <- read_fst("analysis/data/derived-data/ed-ensemble-out.fst") %>%
  as_tibble()
params <- read_fst("analysis/data/derived-data/ed-params.fst") %>%
  as_tibble()

params_sub <- params %>%
  semi_join(
    params %>%
      group_by(pft, variable) %>%
      filter(sd(value) > 0) %>%
      ungroup() %>%
      distinct(variable)
  ) %>%
  rename(
    parameter = variable,
    parameter_value = value
  )

params_wide <- spread(params_sub, variable, value)

ed_summary <- ed_in %>%
  # Growing season
  filter(lubridate::month(time) >= 6, lubridate::month(time) <= 8) %>%
  ## group_by(variable, ymonth = lubridate::floor_date(time, unit = "month")) %>%
  group_by(run_id, variable, yyear = lubridate::year(time)) %>%
  summarize(
    value_mean = mean(value, na.rm = TRUE)
    ## value_lo = quantile(value, 0.025, na.rm = TRUE),
    ## value_hi = quantile(value, 0.975, na.rm = TRUE)
  ) %>%
  ungroup(ed_summary)

time_average <- ed_summary %>%
  filter(yyear > 1912) %>%
  group_by(variable, run_id) %>%
  summarize(value_mean = mean(value_mean)) %>%
  left_join(params_sub)

linmod <- time_average %>%
  group_by(variable, pft, parameter) %>%
  nest() %>%
  mutate(
    fit = map(data, possibly(lm, NULL), formula = value_mean ~ parameter_value),
    failed_fit = map_lgl(fit, is.null),
  ) %>%
  filter(!failed_fit) %>%
  mutate(
    r2 = map2_dbl(fit, data, modelr::rsquare),
    slope = map_dbl(fit, ~coefficients(.x)[[2]])
  )

linmod %>%
  arrange(desc(r2))

time_average %>%
  filter(pft == "Early hardwood",
         parameter == "nonlocal_dispersal") %>%
  ggplot() +
  aes(x = parameter_value, y = value_mean) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~variable, scales = "free_y")

## nested <- time_average %>%
##   group_by(pft, variable) %>%
##   nest()

## all_corr <- nested %>%
##   mutate(corr = map(data, do_corr))
  
## nested[["data"]][[1]]

ed_summary %>%
  left_join(
    params_wide %>%
      filter(pft == "Early hardwood")
  ) %>%
  ggplot() +
  ## aes(x = ymonth, y = value_mean) +
  aes(x = yyear, y = value_mean, group = run_id, color = nonlocal_dispersal) +
  geom_line(alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  scale_color_viridis_c()

ggsave("analysis/figures/prelim-ed-ensemble-out.png",
       width = 8, height = 8)
