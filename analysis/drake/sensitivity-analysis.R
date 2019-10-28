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
    tibble(year = 1920:1950, sgroup = "1920-1950"),
    tibble(year = 1975:1999, sgroup = "1975-1999")
  ),
  params_wide = params %>%
    rename(pft = name) %>%
    mutate(pft = factor(pft, pfts("bety_name"), pfts("shortname"))) %>%
    pivot_longer(-c(param_id, pft)) %>%
    unite(variable, c(pft, name), sep = ".") %>%
    pivot_wider(names_from = "variable", values_from = "value"),
  sensitivity_inputs = both_wide %>%
    inner_join(sensitivity_groups, "year") %>%
    rename(
      EH = `Early hardwood`,
      MH = `Mid hardwood`,
      LH = `Late hardwood`
    ) %>%
    select(case, model_id, param_id, sgroup, sensitivity_vars) %>%
    group_by(case, model_id, param_id, sgroup) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(sensitivity_vars, names_to = "yvar", values_to = "y") %>%
    left_join(params_wide, "param_id") %>%
    pivot_longer(-c(case, model_id, param_id, sgroup, yvar, y),
                 names_to = "xvar", values_to = "x"),
  median_sens_inputs = median_wide %>%
    inner_join(sensitivity_groups, "year") %>%
    select(model_id, sgroup, sensitivity_vars,
           matches(paste(pfts("shortname"), collapse = "|"))) %>%
    group_by(model_id, sgroup) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(
      sensitivity_vars,
      names_to = "yvar",
      values_to = "ymedian"
    ) %>%
    pivot_longer(
      -c(model_id, sgroup, yvar, ymedian),
      names_to = "xvar",
      values_to = "xmedian"
    ),
  combined_sens_inputs = sensitivity_inputs %>%
    left_join(median_sens_inputs, c("model_id", "sgroup", "yvar", "xvar")),
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
    facet_wrap(vars(model), scales = "free", drop = TRUE, ncol = 2,
               dir = "v") +
    scale_color_manual(values = pfts("color")) +
    labs(color = "PFT") +
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
      shortname = factor(shortname, pfts("shortname")),
      model = factor(model_id, c("CTS", "CTP", "CMS", "CMP",
                                 "FTS", "FTP", "FMS", "FMP"))
    ),
  sensitivity_period1_gg =
    sensplot(sensitivity_sub, "1920-1950", fpvar, "mmean_npp_py") +
    labs(y = "Partial variance") +
    guides(color = FALSE),
  sensitivity_period1_png = cowplot::ggsave2(file_out(!!here(
    "analysis", "figures", "partial-variance-npp.png"
  )), sensitivity_period1_gg, width = 16.3, height = 8.4
  )
))

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
