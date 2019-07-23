#########################################
# Which runs match observations?
#########################################
time_averages_all <- time_averages %>%
  ungroup() %>%
  semi_join(time_averages %>%
              ungroup() %>%
              count(param_id, sort = TRUE) %>%
              filter(n >= 6), "param_id") %>%
  select(model, param_id, everything())

# Overall NPP -- 37
time_averages_all %>%
  top_n(10, npp) %>%
  arrange(desc(npp))

# Overall LAI
time_averages_all %>%
  top_n(10, lai) %>%
  arrange(desc(lai))

# Overall diversity
time_averages_all %>%
  top_n(15, shannon) %>%
  arrange(desc(shannon))

# NPP -- 294; 25
time_averages_all %>%
  group_by(model) %>%
  top_n(4, npp) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

# LAI -- 294; 25
time_averages_all %>%
  group_by(model) %>%
  top_n(4, lai) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

# Shannon -- 253
time_averages_all %>%
  group_by(model) %>%
  top_n(2, shannon) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

# Most LAI by PFT
lai_q90_all <- lai_q90 %>%
  semi_join(time_averages_all, "case") %>%
  mutate(param_id = as.numeric(substring(case, 0, 3)))
ta_pft <- lai_q90_all %>%
  filter(year > 1975) %>%
  select(case, model_id, param_id, shortname, lai) %>%
  group_by(case, model_id, param_id, shortname) %>%
  summarize(lai = mean(lai))

# Early hardwood -- 25; 38
ta_pft %>%
  filter(shortname == "Early") %>%
  group_by(model_id) %>%
  top_n(3, lai) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

# Mid hardwood -- 50
ta_pft %>%
  filter(shortname == "Mid") %>%
  group_by(model_id) %>%
  top_n(5, lai) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

ta_pft %>%
  filter(param_id == 50, shortname == "Mid")

# Late hardwood -- 172
ta_pft %>%
  filter(shortname == "Late") %>%
  group_by(model_id) %>%
  top_n(5, lai) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

# Pine -- 172
ta_pft %>%
  filter(shortname == "Mid") %>%
  top_n(5, lai) %>%
  arrange(desc(lai))

  ## ungroup() %>%
  ## count(param_id, sort = TRUE)

# Closest to observations -- 160
time_averages_all %>%
  mutate(lai_o = observations_wide$lai_mean,
         npp_o = observations_wide$npp_mean,
         dlai = lai - lai_o,
         dnpp = npp - npp_o,
         dlair = dlai / lai_o,
         dnppr = dnpp / npp_o,
         dtot = -abs(dlair) - abs(dnppr)) %>%
  select(model, case, dtot, matches("npp|lai")) %>%
  group_by(model) %>%
  arrange(desc(dtot)) %>%
  top_n(3, dtot) %>%
  mutate(param_id = as.numeric(substring(case, 0, 3))) %>%
  ungroup() %>%
  count(param_id, sort = TRUE)

# Closest to observations
time_averages_all %>%
  mutate(lai_o = observations_wide$lai_mean, npp_o = observations_wide$npp_mean,
         dlai = lai - lai_o,
         dnpp = npp - npp_o,
         dlair = dlai / lai_o,
         dnppr = dnpp / npp_o,
         dtot = -abs(dlair) - abs(dnppr)) %>%
  select(model, param_id, dtot, matches("npp|lai")) %>%
  arrange(desc(dtot))
  ## group_by(model) %>%
  ## top_n(1, dtot)

## %>%
##   mutate(param_id = as.numeric(substring(case, 0, 3))) %>%
##   ungroup() %>%
##   count(param_id, sort = TRUE)

# Most productive run with the closed two-stream plastic
time_averages_all %>%
  filter(model == "finite two-stream static") %>%
  arrange(desc(npp))

  ## ggplot() +
  ## geom_point(data = time_averages, aes(x = lai, y = npp), color = "grey90") +
  ## geom_point(data = time_averages_all, aes(x = lai, y = npp), color = "grey70") +
  ## geom_point(aes(x = lai, y = npp)) +
  ## geom_point(aes(x = lai_o, y = npp_o), color = "red") +
  ## facet_wrap(vars(model)) +
  ## theme_cowplot()
