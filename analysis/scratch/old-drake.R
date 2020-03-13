# pairs-plots.R
both_wide %>%
  filter(year == 1920) %>%
  select(model_id, starts_with("mmean"), `Early hardwood`:`Pine`) %>%
  GGally::ggpairs(
    .,
    aes(color = model_id),
    size = 0.5,
    alpha = 0.2,
    columns = names(.)[-1]
  )

tern_data %>%
  filter(year == 1920, model_id == "FMS") %>%
  ggplot() +
  aes(x = Early, y = `Mid/Late`, z = Pine) +
  ggtern::geom_tri_tern(bins = 10) +
  scale_fill_viridis_c(na.value = "white") +
  ggtern::coord_tern()

get_density <- function(x, y, z, ...) {
  dens <- misc3d::kde3d(x, y, z, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  iz <- findInterval(z, dens$z)
  ii <- cbind(ix, iy, iz)
  dens$d[ii]
}

pft_tern_pl

tern_data2 <- tern_data %>%
  group_by(model_id) %>%
  mutate(dens = get_density(`Early hardwood`, `ML`, `Pine`)) %>%
  ungroup()
ggplot(tern_data2) +
  aes(x = `Early hardwood`, y = `ML`, z = `Pine`, color = dens) +
  geom_point() +
  facet_wrap(vars(model_id), ncol = 2) +
  scale_color_viridis_c() +
  ggtern::coord_tern()


pft_data %>%
  mutate(model_id = substr(case, 4, 6)) %>%
  left_join(models, "model_id") %>%
  ggplot() +
  aes(x = year, y = agb_frac, group = case, color = color) +
  geom_line(alpha = 0.2, size = 0.2) +
  facet_grid(vars(pft), vars(fct_relabel(model, ~gsub(" ", "\n", .)))) +
  labs(y = "AGB fraction") +
  scale_color_identity() +
  theme_cowplot() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    strip.background = element_blank()
  )

##################################################
# parameter-analysis.R
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
  filter(year == 1980)

##################################################
# pft-composition-plots.R
dev.size()

### STOP HERE
stop()

saveRDS(sensitivity_raw_results, "analysis/data/retrieved/sensitivity-raw-results.rds")

library(tidyverse)
library(here)
library(fortebaseline)
sensitivity_raw_results <- readRDS(here(
  "analysis",
  "data",
  "retrieved",
  "sensitivity-raw-results.rds"
))

pn2 <- sensplot(sensitivity_sub, "1975-1999", fpvar, "mmean_npp_py") +
  ggtitle("1975-1999") +
  labs(y = "Partial variance")
p12 <- cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.8, 1))
cowplot::ggsave2(
  here("analysis", "figures", "partial-variance-npp.png"),
  pn1, width = 10, height = 8
)

p1 <- sensplot(sensitivity_sub, "1920-1950", fpvar, "mmean_lai_py") +
  ggtitle("1920-1950") +
  labs(y = "Partial variance") +
  guides(color = FALSE)
p2 <- sensplot(sensitivity_sub, "1975-1999", fpvar, "mmean_lai_py") +
  ggtitle("1975-1999") +
  labs(y = "Partial variance")
p12 <- cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.8, 1))
cowplot::ggsave2(
  here("analysis", "figures", "partial-variance-lai.png"),
  p12, width = 16.3, height = 8.4
)

## sensplot(sensitivity_sub, "1920-1950", elasticity)
## sensplot(sensitivity_sub, "1975-1999", elasticity)


sensitivity_sub %>%
  filter(sgroup == "1975-1999", model == "FTP")


slai <- sensitivity_sub %>%
  filter(yvar == "mmean_lai_py") %>%


sensitivity_proc %>%
  ungroup() %>%
  filter(sgroup == sgroup[1]) %>%
  group_by(model_id, yvar) %>%
  arrange(desc(pvar), .by_group = TRUE) %>%
  slice(1:5) %>%
  separate(xvar, c("pft", "param"), extra = "merge")


sensitivity_proc %>%
  ungroup() %>%
  filter(is.finite(elasticity)) %>%
  arrange(desc(pvar))

summary(sensitivity_proc)

s <- combined_sens_inputs %>%
  group_by(model_id, sgroup, yvar, xvar) %>%
  group_split()

s1 <- s[[1]]
s1 %>%
  select(y, x, ymedian, xmedian) %>%
  pmap()
sensitivity_analysis(s1$y, s1$x, s1$ymedian[1], s1$xmedian[1])


sensitivity_results = sensitivity_inputs %>%
  group_by(model_id, sgroup, yvar, xvar) %>%
  summarize(sens_out = sensitivity_analysis(y, x, ))

both_wide

model <- both_wide$model[1]
year <- 1920

median_yr = median_wide %>%
  filter(year == !!year, model == !!model) %>%
  select(param_id, year, !!sensitivity_vars)


s <- sensitivity_analysis()

both_wide

dsub <- both_wide %>%
  filter(model == !!model, year == !!year) %>%
  select(
    param_id,
    mmean_npp_py, mmean_lai_py,
    EH = `Early hardwood`,
    MH = `Mid hardwood`,
    LH = `Late hardwood`,
    Pine
  ) %>%
  left_join()

##################################################
# specific-parameters.R

### STOP HERE
stop()

dev.size()

all_params <- both_wide %>%
  filter(year == 1999) %>%
  mutate(param_id = as.numeric(substr(case, 0, 3))) %>%
  count(param_id) %>%
  filter(n == 8)

both_wide %>%
  mutate(param_id = as.numeric(substr(case, 0, 3))) %>%
  semi_join(all_params, "param_id") %>%
  rename(EH = `Early hardwood`, MH = `Mid hardwood`, LH = `Late hardwood`) %>%
  rename_all(~gsub("mmean_", "", .x)) %>%
  rename_all(~gsub("_py", "", .x)) %>%
  mutate(
    bbeta = dbeta(EH, 2, 2) + dbeta(MH, 2, 2) +
      dbeta(LH, 2, 2) + dbeta(Pine, 2, 2)
  ) %>%
  filter(year == 1999, bbeta > 3) %>%
  arrange(desc(npp)) %>%
  select(case, bbeta, EH, MH, LH, Pine, npp = npp, everything())


both_wide %>%
  mutate(param_id = as.numeric(substr(case, 0, 3))) %>%
  filter(param_id == 448)

##################################################
# structure-results.R

# Look at each model indidivually
dcrown <- default_results %>%
  filter(!multiple_scatter, !trait_plasticity) %>%
  mutate(crown_model = if_else(crown_model, "finite", "closed") %>%
           factor(c("closed", "finite")))

dcrown_sa <- dcrown %>%
  select(crown_model, scalar) %>%
  unnest(scalar) %>%
  select(-c(case:param_id)) %>%
  pivot_longer(-c(crown_model, datetime)) %>%
  annual_mean()

dcrown_sa %>%
  filter(name %in% c("mmean_gpp_py", "mmean_npp_py")) %>%
  ggplot() +
  aes(x = year, y = value, color = crown_model, group = crown_model) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y")

dcrown_pa <- dcrown %>%
  select(crown_model, pft_py) %>%
  unnest(pft_py) %>%
  select(-c(case:param_id)) %>%
  pivot_longer(-c(crown_model:pft)) %>%
  filter(month(datetime) %in% 6:8) %>%
  annual_mean() %>%
  mutate(pft = set_pft(pft))

dcrown_pa %>%
  filter(name == "mmean_bstorage_py") %>%
  ggplot() +
  aes(x = year, y = value, color = crown_model, group = crown_model) +
  geom_line() +
  facet_grid(vars(pft), scales = "free_y")

dcrown_ca <- dcrown %>%
  select(crown_model, cohort) %>%
  unnest(cohort) %>%
  select(-c(case:param_id))

## dcrown_ca %>%
default_results %>%
  select(casename, cohort) %>%
  unnest(cohort) %>%
  filter(
    month(datetime) == 7,
    year(datetime) %in% c(1910, 1920, 1950, 1980)
  ) %>%
  mutate(pft = set_pft(pft),
         datetime = year(datetime)) %>%
  ggplot() +
  aes(x = dbh, y = hite, color = pft) +
  geom_segment(aes(xend = 0, yend = hite)) +
  geom_point() +
  facet_grid(vars(datetime), vars(casename))

##################################################
# zz-old.R

plan <- bind_plans(plan, drake_plan(
  #########################################
  # Summary time series
  #########################################
  use_vars = c("gpp", "npp", "agb", "lai", "shannon"),
  use_vars_cap = c("GPP", "NPP", "AGB", "LAI", "Shannon"),
  variable_cols = c("case", "datetime", "pft", "nplant",
                    "bleaf", "bsapwooda", "bstorage",
                    "fmean_gpp_co", "fmean_npp_co", "lai_co"),
  scalar_cols = c("case", "model_id", "param_id", "datetime",
                  "mmean_gpp_py", "mmean_rh_py", "mmean_plresp_py"),
  scalar_means = setDT(fst(file_in(!!mscalar_file))[, scalar_cols]) %>%
    .[, `:=`(npp = mmean_gpp_py - mmean_plresp_py,
             nee = mmean_gpp_py - mmean_plresp_py - mmean_rh_py)] %>%
    # Annual value (sum), and kgC/m2 -> MgC/ha (x10)
    .[, lapply(.SD, function(x) sum(x * 10)),
      by = .(case, model_id, param_id, year = year(datetime)),
      .SDcols = c("mmean_gpp_py", "mmean_rh_py", "mmean_plresp_py",
                  "npp", "nee")],
  pft_cols = c("case", "model_id", "param_id", "pft", "datetime",
               "agb_py", "mmean_lai_py", "nplant_py"),
  pft_means = setDT(fst(mpft_file)[, pft_cols]) %>%
    # Restrict to growing season because LAI is zero otherwise
    .[between(month(datetime), 6, 8),
      .(agb = mean(agb_py), lai = mean(mmean_lai_py), nplant = mean(nplant_py)),
      .(case, model_id, param_id, pft, year = year(datetime))],
  pft_aggregates = pft_means %>%
    .[, lapply(.SD, sum), .(case, model_id, param_id, year),
      .SDcols = c("agb", "lai", "nplant")],
  plot_means = scalar_means[pft_aggregates,
                            on = c("case", "model_id", "param_id", "year")],
  plot_means_long = plot_means %>%
    rename_all(~gsub("^mmean_", "", .)) %>%
    rename_all(~gsub("_py$", "", .)) %>%
    melt(id.vars = c("case", "model_id", "param_id", "year")),
  ## plot_means2 = setDT(fst(file_in(!!mcohort_file))[, variable_cols])[j = .(
  ##   agb = sum((bleaf + bsapwooda + bstorage) * nplant),
  ##   gpp = sum(fmean_gpp_co * nplant),
  ##   npp = sum(fmean_npp_co * nplant),
  ##   lai = sum(lai_co)
  ## ), by = c("case", "datetime")] %>%
  ##   as_tibble() %>%
  ##   mutate(model_id = substr(case, 4, 6)),
  tsplot_vars = c("gpp", "npp", "plresp", "lai"),
  tsplot_labeller = labeller(
    model = function(x) gsub(" ", "\n", x),
    variable = as_labeller(c(
      "gpp" = "GPP ~ (MgC ~ ha^-1 ~ year^-1)",
      "npp" = "NPP ~ (MgC ~ ha^-1 ~ year^-1)",
      "plresp" = "RA ~ (MgC ~ ha^-1 ~ year^-1)",
      "lai" = "LAI"
    ), default = label_parsed),
    .default = label_value
  ),
  tsplot_data = plot_means_long %>%
    .[variable %in% tsplot_vars, ] %>%
    .[, variable := factor(variable, tsplot_vars)] %>%
    .[as.data.table(models), on = "model_id"],
  tsplot_summary = tsplot_data %>%
    .[, .(mid = mean(value),
          lo = quantile(value, 0.05),
          hi = quantile(value, 0.95)),
      .(model, year, variable)],
  # Calculate diversity indices based on LAI
  diversity = as.data.table(fst(!!cohort_file)[, c("case", "datetime",
                                                   "pft", "lai_co")]) %>%
    .[, year := year(datetime)] %>%
    .[, .(lai_co = quantile(lai_co, 0.9)), .(case, year, pft)] %>%
    .[, lai_p := lai_co / sum(lai_co), .(case, year)] %>%
    .[lai_p > 0] %>%
    .[, .(shannon = -sum(lai_p * log(lai_p))), .(case, year)] %>%
    as_tibble() %>%
    mutate(model_id = substr(case, 4, 6)),
  jja_means = plot_means %>%
    filter(month(datetime) %in% 6:8) %>%
    group_by(case, model_id, year = year(datetime)) %>%
    summarize_at(vars(-datetime), mean) %>%
    ungroup() %>%
    left_join(diversity, by = c("case", "model_id", "year")) %>%
    mutate(shannon = if_else(is.na(shannon), 0, shannon)) %>%
    inner_join(models, by = "model_id"),
  jja_long = jja_means %>%
    gather(variable, value, agb:shannon) %>%
    mutate(variable = factor(variable, use_vars),
           param_id = as.numeric(substr(case, 1, 3))),
  jja_summary = jja_long %>%
    group_by(model, color, variable, year) %>%
    summarize(avg = mean(value),
              lo = quantile(value, 0.1),
              hi = quantile(value, 0.9)),
  my_labeller = labeller(
    model = function(x) gsub(" ", "\n", x),
    variable = as_labeller(c(
      "gpp" = "GPP ~ (MgC ~ ha^-1 ~ year^-1)",
      "npp" = "NPP ~ (MgC ~ ha^-1 ~ year^-1)",
      "agb" = "AGB ~ (kgC)",
      "lai" = "LAI",
      "shannon" = "Shannon ~ diversity"
    ), default = label_parsed),
    .default = label_value
  ),
  observations = tribble(
    ~variable, ~low, ~mean, ~hi, ~source,
    ## "lai", 1.8, 4.14, 6.56, "Hardiman 2013",
    ## "npp", 1.68, 3.11, 7.26, "Hardiman 2013",
    # TODO: These are ballpark numbers from Gough 2008 AFM. Replace with more precise numbers
    "npp", 6, 6.5, 7, "UMBS",
    "lai", 3.97 - 1.96 * 0.423, 3.97, 3.97 + 1.96 * 0.423, "Ameriflux"
  ) %>% mutate(variable = factor(variable, use_vars),
               year = max(jja_summary$year)),
  observations_wide = observations %>%
    select(-year, -source) %>%
    gather(stat, value, low, mean, hi) %>%
    unite("variable", variable, stat) %>%
    spread(variable, value),
  summary_ts_plot = ggplot(tsplot_data) +
    aes(x = year) +
    geom_line(aes(y = value, group = param_id, color = color), alpha = 0.25) +
    geom_line(data = tsplot_summary, aes(y = hi),
              color = "black", linetype = "dashed") +
    geom_line(data = tsplot_summary, aes(y = lo),
              color = "black", linetype = "dashed") +
    geom_line(aes(y = mid), color = "black", size = 1, data = tsplot_summary) +
    ## geom_pointrange(data = observations,
    ##                 aes(y = mean, ymin = low, ymax = hi),
    ##                 color = "black") +
    scale_color_identity() +
    facet_grid(vars(variable), vars(model), scales = "free_y",
               switch = "y", labeller = tsplot_labeller) +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.placement = "outside",
          strip.background = element_blank()),
  summary_ts_plot_png = ggsave(
    file_out(!!path(fig_dir, "summary-ts-plot.png")),
    summary_ts_plot,
    # TODO: Fix dims so axis titles aren't cut off
    width = 10, height = 10
  ),
  summary_ts_plot_knit = knitr::include_graphics(file_in(!!path(
    fig_dir, "summary-ts-plot.png"
  ))),
  #########################################
  # LAI by PFT plot
  #########################################
  cohort_lai = fst(file_in(!!cohort_file)) %>%
    .[, c("case", "datetime", "pft", "lai_co")] %>%
    as_tibble(),
  lai_q90 = setDT(fst(file_in(!!cohort_file))[, j = c(
    "case", "datetime", "pft", "lai_co")]) %>%
    .[, .(lai = sum(lai_co)), .(case, datetime, pft)] %>%
    .[, .(lai = quantile(lai, 0.9)), .(case, year = year(datetime), pft)] %>%
    .[, model_id := substr(case, 4, 6)] %>%
    as_tibble() %>%
    rename(num = pft) %>%
    left_join(pfts(), by = "num"),
  matches_observed = time_averages %>%
    ungroup() %>%
    filter(lai >= observations_wide$lai_low,
           lai <= observations_wide$lai_hi,
           npp >= observations_wide$npp_low,
           npp <= observations_wide$npp_hi) %>%
    distinct(case, param_id),
  # See analysis/scratch/param-subsets.R for origins
  my_subsets = tribble(
    ~param_id, ~why,
    ## 37, "Overall high NPP",
    31, "Relatively high LAI; close to observation; competitive",
    25, "High NPP and LAI; closest to observation",
    91, "Overall high LAI; pine dominanted",
    50, "Overall high diversity",
    160, "Overall closest to observation",
  ) %>% mutate(label = LETTERS[seq_len(n())]),
    ## 31, "High NPP and LAI; closest to observation",
    ## 25, "High NPP and LAI; closest to observation",
    ## 180, "High NPP and LAI; early-mid competition",
    ## 172, "High NPP and LAI; early-mid competition",
    ## 169, "High diversity",
    ## 160, "High pine",
  ## my_subsets = tribble(
  ##   ~param_id, ~why, ~label,
  ##   294, "High NPP and LAI; high pine", "A",
  ##   295, "High diversity", "B",
  ##   160, "Close to observations", "C",
  ##   25, "High early hardwood", "D",
  ##   # No runs have particularly high mid-hardwood LAI
  ##   ## 50, "High mid hardwood", "E",
  ##   172, "High late hardwood", "E"
  ## ),
  lai_pft_plot = lai_q90 %>%
    inner_join(models, by = "model_id") %>%
    mutate(param_id = as.numeric(substring(case, 0, 3))) %>%
    inner_join(my_subsets, "param_id") %>%
    ggplot() +
    aes(x = year, y = lai, color = pft) +
    geom_line() +
    facet_grid(vars(label), vars(model), labeller = my_labeller) +
    scale_color_manual(values = pfts("color")) +
    labs(y = "Leaf area index", color = "PFT") +
    theme_cowplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    ),
  lai_pft_plot_png = ggsave(
    file_out(!!path(fig_dir, "lai-pft-plot.png")),
    lai_pft_plot,
    width = 11.42, height = 5.67
  ),
  lai_pft_plot_knit = knitr::include_graphics(file_in(!!path(
    fig_dir, "lai-pft-plot.png"
  ))),
  #########################################
  # Parameter vs. structure uncertainty
  #########################################
  time_averages = jja_means %>%
    filter(year > 1975) %>%
    group_by(model, color, case) %>%
    summarize_at(use_vars, mean) %>%
    mutate(param_id = as.numeric(substr(case, 1, 3))),
  parameter_uncertainty = time_averages %>%
    group_by(model, color) %>%
    summarize_at(use_vars, var) %>%
    ungroup() %>%
    arrange(model),
  structure_uncertainty = time_averages %>%
    group_by(model) %>%
    summarize_at(use_vars, mean) %>%
    ungroup() %>%
    summarize_at(vars(-model), var) %>%
    mutate(model = "Across-structure",
           color = "black"),
  both_uncertainty = parameter_uncertainty %>%
    mutate(model = as.character(model)) %>%
    bind_rows(structure_uncertainty) %>%
    mutate(model = fct_inorder(model)),
  within_across_plot = both_uncertainty %>%
    gather(variable, variance, -model, -color) %>%
    mutate(variable = factor(variable, use_vars) %>%
             lvls_revalue(use_vars_cap)) %>%
    ggplot() +
    aes(x = model, y = variance, fill = model) +
    geom_col() +
    facet_wrap(vars(variable), scales = "free_y") +
    labs(x = "Model structure", y = "Variance") +
    scale_fill_manual(
      values = tibble::deframe(both_uncertainty[, c("model", "color")])
    ) +
    guides(fill = guide_legend(ncol = 1)) +
    theme_cowplot() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.7, 0.28)
    ),
  within_across_plot_png = ggsave(
    file_out(!!path(fig_dir, "within-across-plot.png")),
    within_across_plot,
    width = 8.3, height = 5.1
  ),
  within_across_plot_knit = knitr::include_graphics(file_in(!!path(
    fig_dir, "within-across-plot.png"
  ))),
  #########################################
  # Sensitivity analysis
  #########################################
  run_params = file_in(!!ensemble_params_file) %>%
    read_csv(col_types = cols(name = col_character(),
                              .default = col_double())) %>%
    rename(bety_name = name) %>%
    left_join(pfts() %>%
                select(bety_name, pft, shortname),
              by = "bety_name"),
  sensitivity_inputs = run_params %>%
    inner_join(time_averages, by = "param_id") %>%
    gather(yvar, yvalue, one_of(use_vars)) %>%
    select(-color, -case, -bety_name, -pft) %>%
    gather(xvar, xvalue,
           -param_id, -shortname,
           -model, -yvar, -yvalue),
  sensitivity_results = sensitivity_inputs %>%
    group_by(model, shortname, xvar, yvar) %>%
    summarize(raw_sa = list(possibly(sensitivity_analysis, NULL)(yvalue, xvalue))) %>%
    filter(map_lgl(raw_sa, negate(is.null))) %>%
    unnest(raw_sa) %>%
    ungroup(),
  sensitivity_plot_data = sensitivity_results %>%
    ## mutate(elasticity = if_else(abs(elasticity) > 75,
    ##                             75 * sign(elasticity),
    ##                             elasticity)) %>%
    group_by(model, yvar) %>%
    mutate(fpvar = pvar / sum(pvar)) %>%
    group_by(xvar) %>%
    mutate(total_pvar = sum(pvar)) %>%
    ungroup() %>%
    mutate(
      trait_alpha = factor(xvar) %>% fct_rev(),
      trait_pvar = fct_reorder(factor(xvar), total_pvar),
      model = fct_relabel(model, gsub, pattern = " ", replacement = "\n"),
      shortname = factor(shortname, pfts("shortname")),
      yvar = factor(yvar, use_vars) %>%
        lvls_revalue(use_vars_cap)
    ),
  sensitivity_plot_piece = target(
    top_n_sensitivity_plot(sensitivity_plot_data, .y_var, .metric,
                           scales = "free") +
      ggtitle(.y_var) +
      labs(y = .ylab, x = "Trait", color = "PFT") +
      scale_color_manual(values = pfts("color")) +
      theme_cowplot() +
      theme(
        axis.text.x = element_text(size = rel(0.5)),
        legend.position = "bottom"
      ),
    transform = map(.y_var = !!rep(c("NPP", "LAI"), 2),
                    .metric = c(elasticity, elasticity, pvar, pvar),
                    .ylab = !!rep(c("Elasticity", "Partial variance"),
                                each = 2))
  ),
  sensitivity_plot = target(
    cowplot::plot_grid(sensitivity_plot_piece, ncol = 2),
    transform = combine(sensitivity_plot_piece)
  ),
  sensitivity_plot_png = ggsave(
    file_out(!!path(fig_dir, "sensitivity-plot.png")),
    sensitivity_plot,
    # TODO: This is huge...
    width = 20, height = 15
  ),
  sensitivity_plot_knit = knitr::include_graphics(file_in(!!path(
    fig_dir, "sensitivity-plot.png"
  ))),
  #########################################
  # Pairs plot of time-averaged values
  #########################################
  my_subsets_time_avg = time_averages %>%
    ungroup() %>%
    inner_join(my_subsets, "param_id"),
  pairs_time_averaged = time_averages %>%
    ungroup() %>%
    ggplot() +
    aes(x = lai, y = npp, color = model) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    ## geom_rug(color = "black", alpha = 0.5) +
    geom_point(data = observations_wide,
               aes(x = lai_mean, y = npp_mean),
               size = 3,
               shape = 17,
               color = "black",
               inherit.aes = FALSE) +
    geom_rect(data = observations_wide,
              aes(xmin = lai_low, xmax = lai_hi,
                  ymin = npp_low, ymax = npp_hi),
              fill = NA,
              linetype = "dashed",
              color = "black",
              inherit.aes = FALSE) +
    geom_text_repel(data = my_subsets_time_avg,
                    aes(label = label),
                    color = "black",
                    min.segment.length = 0) +
    scale_color_manual(values = model_colors) +
    coord_cartesian(xlim = c(0, 9), ylim = c(0, 11)) +
    labs(x = "LAI", y = expression(NPP ~ (MgC ~ ha^-1 ~ year ^ -1))) +
    ## guides(color = guide_legend(nrow = 4)) +
    guides(color = FALSE) +
    ## facet_wrap(vars(model), scales = "fixed", labeller = my_labeller) +
    facet_wrap(vars(model), scales = "fixed", ncol = 4, dir = "h") +
    theme_cowplot() +
    theme(legend.position = "bottom",
          strip.background = element_blank()),
  pairs_time_averaged_png = ggsave(
    file_out(!!path(fig_dir, "pairs-time-averaged.png")),
    pairs_time_averaged,
    height = 5.67,
    width = 8.92
  ),
  pairs_time_averaged_knit = knitr::include_graphics(file_in(!!path(
    fig_dir, "pairs-time-averaged.png"
  ))),
))
