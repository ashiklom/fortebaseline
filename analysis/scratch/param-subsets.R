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

##################################################
params_wide2 <- params_wide %>%
  mutate_at(vars(ends_with("water_conductance")), log10)

param_diffs_df <- function(dat) {
  sdat <- distinct(dat, case, param_id, model, crown, rtm, traits)
  params_wide2 %>%
    mutate(in_dat = param_id %in% sdat$param_id)
}

param_diffs <- function(dat) {
  params_sdat <- param_diffs_df(dat)
  params_sdat_long <- pivot_longer(params_sdat, -c(param_id, in_dat))
  mean_diff <- params_sdat_long %>%
    ## group_by(name) %>%
    ## mutate(all_avg = mean(value)) %>%
    ## ungroup() %>%
    group_by(in_dat, name) %>%
    summarize(avg = mean(value)) %>%
    pivot_wider(names_from = in_dat, values_from = avg) %>%
    mutate(reldiff = (`TRUE` - `FALSE`) / (`TRUE` + `FALSE`))
  lvls <- mean_diff %>%
    arrange(desc(reldiff)) %>%
    pull(name)
  params_sdat_long %>%
    separate(name, c("PFT", "trait"), sep = "\\.", extra = "merge",
             remove = FALSE) %>%
    ## mutate(name = factor(name, lvls)) %>%
    ggplot() +
    aes(x = in_dat, y = value) +
    geom_violin() +
    geom_jitter(size = 0.1, alpha = 0.3) +
    stat_summary(fun.y = mean, color = "red", geom = "point") +
    ## facet_wrap(vars(name), scales = "free_y")
    facet_wrap(vars(PFT, trait), scales = "free",
               nrow = 4, ncol = 26) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          strip.text = element_text(size = 4))
}

diffdat <- both_wide %>%
  mutate(npft_eff = 1 / (`Early hardwood`^2 + `Mid hardwood`^2 + `Late hardwood`^2 + Pine^2))

last_ten <- diffdat %>%
  filter(year > 1990) %>%
  group_by_at(vars(param_id:traits)) %>%
  summarize_at(vars(ends_with("_py"), ends_with("hardwood"), Pine, npft_eff), mean) %>%
  ungroup()

last_ten %>%
  filter(
    mmean_npp_py >= 6, mmean_npp_py <= 7,
    mmean_lai_py >= 3.14, mmean_lai_py <= 4.80
  ) %>%
  count(model)

last_ten %>%
  arrange(desc(npft_eff)) %>%
  top_n(10)

loadd(trait_distribution)

good_cases <- good_dat %>%
  distinct(param_id, model)

good_cases %>%
  count(model)

good_params <- params_wide %>%
  inner_join(good_cases, "param_id") %>%
  select(model, param_id, everything())

good_mat <- good_params %>%
  filter(model == model[[1]]) %>%
  mutate_at(vars(ends_with("water_conductance")), log10) %>%
  select_if(~any(!is.na(.))) %>%
  select(-model, -param_id)

good_pc <- princomp(good_mat)

good_params_long <- good_params %>%
  ## mutate_at(vars(ends_with("water_conductance")), log10) %>%
  pivot_longer(-c(param_id, model), names_to = "variable", values_to = "value") %>%
  extract(variable, c("PFT", "trait"), "(.*?)\\.(.*)")

td <- trait_distribution %>%
  select(PFT = shortname, trait, distn, parama, paramb) %>%
  mutate(
    pfun = map(paste0("p", distn), rlang::as_function),
    pfun = pmap(list(pfun, parama, paramb), function(f, a, b) function(x) f(x, a, b))
  ) %>%
  select(PFT, trait, pfun)

td2 <- good_params_long %>%
  left_join(td, c("PFT", "trait")) %>%
  mutate(pval = map2_dbl(pfun, value, possibly(rlang::exec, NA_real_)))

td2 %>%
  filter(pval < 0.05 | pval > 0.95) %>%
  count(model, PFT, trait, sort = TRUE)

td2 %>%
  filter(model == model[[1]]) %>%
  pull(pval) %>%
  hist()

td_summary <- td2 %>%
  group_by(model, PFT, trait) %>%
  summarize(
    Count = n(),
    Mean = mean(pval),
    Min = min(pval),
    Max = max(pval),
    Range = Max - Min
  ) %>%
  ungroup()

td_summary %>%
  group_by(PFT, trait) %>%
  summarize(vmean = var(Mean)) %>%
  ungroup() %>%
  arrange(desc(vmean))

td2 %>%
  filter(PFT == "Pine", trait == "water_conductance") %>%
  ggplot() +
  aes(x = model, y = pval) +
  geom_point()

  filter(model == "finite two-stream static") %>%
  arrange(Range)

td2 %>%
  filter(model == model[[1]]) %>%
  arrange(pval)

good_params_pft <- good_params_long %>%
  mutate(PFT = factor(PFT, pfts("shortname"))) %>%
  arrange(PFT) %>%
  pivot_wider(names_from = "PFT", values_from = "value") %>%
  mutate(paramtype = "good")

all_params_pft <- params_wide %>%
  mutate_at(vars(ends_with("water_conductance")), log10) %>%
  pivot_longer(-param_id, names_to = "variable", values_to = "value") %>%
  extract(variable, c("PFT", "trait"), "(.*?)\\.(.*)") %>%
  pivot_wider(names_from = "PFT", values_from = "value") %>%
  mutate(paramtype = "all")

good_all_params <- bind_rows(all_params_pft, good_params_pft) %>%
  mutate(paramtype = fct_inorder(paramtype))

pdf("analysis/figures/zz-trait-pairs.pdf")
for (t in unique(good_params_pft$trait)) {
  d <- good_all_params %>%
    filter(trait == !!t) %>%
    select(Early:Pine, paramtype)
  tryCatch({
    palette(c("gray80", "black"))
    pairs(d[,1:4], main = t, col = as.numeric(d[[5]]),
          pch = 19)
  }, error = function(e) {
      message("Error for trait ", t)
      message(conditionMessage(e))
    }
  )
}
dev.off()

good_params_pft

param_diffs(good_dat)

diffdat %>%
  semi_join(good_dat, "case") %>%
  mutate(prod_eff = mmean_npp_py / mmean_lai_py) %>%
  ## select(param_id, year, model, matches("mmean_(npp|lai)"), prod_eff, npft_eff,
  ##        `Early hardwood`, `Mid hardwood`, `Late hardwood`, Pine) %>%
  select(param_id, year, model, matches("mmean_(npp|lai)"), prod_eff, npft_eff) %>%
  pivot_longer(-c(param_id, year, model)) %>%
  mutate(name = fct_inorder(name)) %>%
  filter(!is.na(model)) %>%
  ggplot() +
  aes(x = year, y = value, group = factor(param_id)) +
  geom_line() +
  facet_grid(vars(name), vars(model), scales = "free_y")

diverse_dat %>%
  distinct(case, model, crown, rtm, traits) %>%
  count(crown, rtm, traits)

params_diverse <- diverse_dat %>%
  param_diffs_df() %>%
  group_by(in_dat) %>%
  summarize_at(
    vars(-c(param_id)),
    list(Mean = mean, SD = sd)
  ) %>%
  pivot_longer(-in_dat, "parameter", "value") %>%
  extract(parameter, c("PFT", "trait", "stat"), "(^.*)\\.(.*)_([[:alpha:]]+$)") %>%
  pivot_wider(names_from = c("in_dat", "stat"), values_from = "value")

params_diverse %>%
  mutate(reldiff = (TRUE_Mean - FALSE_Mean) / TRUE_SD) %>%
  arrange(desc(abs(reldiff))) %>%
  print(n = 20)


params_diverse %>%




diffdat %>%
  filter(year > 1980, npft_eff > 2) %>%
  param_diffs()

# Let's look at 1920
y1920 <- both_wide %>%
  filter(year == 1920)

ggplot(y1920) +
  aes(x = 1, y = `mmean_gpp_py`) +
  geom_violin() +
  geom_jitter(size = 0.2, alpha = 0.3)

both_wide %>%
  filter(year == 1980) %>%
  ggplot() +
  aes(x = model, y = mmean_gpp_py) +
  geom_violin()

both_wide %>%
  filter(year == 1980, `Mid hardwood` > 0.75) %>%
  param_diffs()

observations

# Closest match to UMBS
both_wide %>%
  filter(year > 1990, `mmean_npp_py` >= 6, `mmean_npp_py` <= 7,
         mmean_lai_py > 3.14, mmean_lai_py < 4.80) %>%
  param_diffs()

y1920 %>%
  filter(mmean_gpp_py > 15) %>%
  param_diffs()

# Which runs like a particular PFT
eh <- y1920 %>%
  filter(`Early hardwood` > 0.75) %>%
  distinct(case, param_id, model, crown, rtm, traits)
params_eh <- params_wide %>%
  ## select(param_id, starts_with("Early")) %>%
  select_if(~any(!is.na(.x))) %>%
  mutate(is_eh = param_id %in% eh$param_id) %>%
  mutate_at(vars(ends_with("water_conductance")), log)

params_eh %>%
  ## filter(logWC > -10) %>%
  pivot_longer(-c(param_id, is_eh)) %>%
  ggplot() +
  aes(x = is_eh, y = value, color = is_eh) +
  geom_violin() +
  geom_jitter(size = 0.1, alpha = 0.3) +
  facet_wrap(vars(name), scales = "free_y")

# Key takeaway: Water conductance drives PFT composition -- when water
# conductance is high, that PFT ends up dominating

# Can we get a publication-quality figure out of this?
dat <- y1920 %>%
  select(param_id:traits, `Early hardwood`:Pine) %>%
  left_join(select(params_wide, param_id, ends_with("water_conductance")),
            "param_id") %>%
  mutate_at(vars(ends_with("water_conductance")), log)

ggplot(dat) +
  aes(x = `Early.water_conductance`, y = `Early hardwood`) +
  geom_point() +
  facet_wrap(vars(model))
# What about NPP?

params_eh %>%
  select(-param_id, -is_eh) %>%
  pairs(pch = ".", gap = 0.3,
        col = ifelse(params_eh$is_eh, "black", "grey70"))

params_wide %>%
  semi_join(eh, "param_id") %>%
  select(starts_with("Early")) %>%
  select_if(~any(!is.na(.x))) %>%
  pairs(pch = ".", col = "red", add = TRUE)
