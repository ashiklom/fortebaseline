fix_cases <- function(x) {
  x2 <- gsub("^.*-", "", x)
  mod_levels <- c("CTS", "CTP", "CMS", "CMP",
                  "FTS", "FTP", "FMS", "FMP")
  factor(x2, mod_levels)
}

default_results_file <- here::here("analysis", "data",
                                     "retrieved", "structure-default.rds")

### Download files from OSF
default_results_osf <- "q9cpf"
plan <- bind_plans(plan, drake_plan(
  default_results_dl = target(
    download.file(osf_url(default_results_osf),
                  file_out(!!default_results_file)),
    trigger = trigger(change = get_timestamp(default_results_osf))
  )
))

### Processing default outputs
plan <- bind_plans(plan, drake_plan(
  default_results = file_in(!!default_results_file) %>%
    readRDS() %>%
    mutate(
      runtype = "default",
      casename = fix_cases(casename)
    ),
  default_annual_scalar = default_results %>%
    select(runtype, casename, scalar) %>%
    unnest(scalar) %>%
    select(-c(case:param_id), -c(age:area_si),
           -c(ndcycle:yatm)) %>%
    group_by(runtype, casename, year = year(datetime)) %>%
    select(-datetime) %>%
    summarize_all(mean) %>%
    ungroup(),
  default_annual_pft = default_results %>%
    select(runtype, casename, pft_py) %>%
    unnest(pft_py) %>%
    select(-c(case:param_id)) %>%
    mutate(pft = set_pft(pft)) %>%
    # Midsummer LAI
    filter(month(datetime) == 8) %>%
    group_by(runtype, casename, year = year(datetime), pft) %>%
    summarize_all(mean) %>%
    ungroup(),
  default_annual_lai = default_annual_pft %>%
    group_by(runtype, casename, year) %>%
    summarize(lai = sum(mmean_lai_py)) %>%
    ungroup(),
  default_annual = default_annual_scalar %>%
    left_join(default_annual_lai, c("runtype", "casename", "year"))
))

### Structure NPP and LAI figure
plan <- bind_plans(plan, drake_plan(
  structure_default_data = default_annual %>%
    select(runtype, model_id = casename, year,
           npp = mmean_npp_py, lai) %>%
    left_join(models, "model_id") %>%
    # Convert kgC m-2 yr-1 to MgC ha-1 yr-1
    mutate(npp = npp * 10) %>%
    pivot_longer(c(npp, lai)),
  structure_compare_default_gg = ggplot(structure_default_data) +
    aes(x = year, y = value, color = model) +
    geom_line() +
    guides(color = guide_legend(title = "Model",
                                override.aes = list(size = 2))) +
    facet_grid(
      vars(name), vars(rtm),
      scales = "free_y",
      switch = "y",
      labeller = labeller(name = as_labeller(c(
        "npp" = "NPP ~ (MgC ~ ha^-1 ~ yr^-1)",
        "lai" = "LAI"
      ), default = label_parsed))
    ) +
    scale_color_manual(values = model_colors) +
    cowplot::theme_cowplot() +
    theme(axis.title = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside"),
  structure_compare_default_png = ggsave(
    file_out("analysis/figures/structure-compare-default.png"),
    structure_compare_default_gg,
    width = 10.1, height = 7
  ),
  structure_compare_default_knit = knitr::include_graphics(file_in(
    "analysis/figures/structure-compare-default.png"
  ))
))

### Light levels by model type figure
plan <- bind_plans(plan, drake_plan(
  structure_default_light_gg = default_results %>%
    select(runtype, casename, cohort) %>%
    unnest(cohort) %>%
    filter(
      month(datetime) == 7,
      year(datetime) %in% c(1910, 1920, 1950, 1980)
    ) %>%
    mutate(
      pft = set_pft(pft),
      datetime = year(datetime)
    ) %>%
    ggplot() +
    aes(x = mmean_light_level_co, y = hite) +
    geom_line() +
    geom_point(aes(color = pft)) +
    facet_grid(vars(casename), vars(datetime)) +
    theme_bw() +
    labs(
      x = "Relative light level",
      y = "Cohort height (m)",
      color = "PFT"
    ) +
    scale_color_manual(values = pfts("color")) +
    theme(legend.position = "bottom"),
  structure_default_light_png = ggsave(
    file_out("analysis/figures/default-light-levels.png"),
    structure_default_light_gg,
    width = 7.16, height = 5.96
  ),
  structure_default_light_knit = knitr::include_graphics(file_in(
    "analysis/figures/default-light-levels.png"
  ))
))

### Now compare default and median parameters
annual_mean <- function(dat) {
  dat %>%
    mutate(year = lubridate::year(datetime)) %>%
    select(-datetime) %>%
    group_by_at(vars(-value)) %>%
    summarize_all(mean) %>%
    ungroup()
}

plan <- plan <- bind_plans(plan, drake_plan(
  median_results = here("analysis", "data", "retrieved",
                        "structure-median.rds") %>%
    readRDS() %>%
    mutate(
      runtype = "median",
      casename = fix_cases(casename)
    ),
  both_results = bind_rows(default_results, median_results),
  both_long_s = both_results %>%
    select(casename, runtype, scalar) %>%
    unnest(scalar) %>%
    select(-c(case:param_id), -age, -area, -area_si,
           -latitude, -longitude, -starts_with("mmsq")) %>%
    pivot_longer(-c(casename:datetime)) %>%
    annual_mean(),
  default_median_fluxes_gg = both_long_s %>%
    filter(grepl("mmean_(gpp|npp|plresp|rh)_py", name)) %>%
    mutate(
      name = stringr::str_remove(name, "mmean_") %>%
        stringr::str_remove("_py") %>%
        factor(c("gpp", "plresp", "npp", "rh"), c("GPP", "RA", "NPP", "RH")),
      # Unit conversion
      value = value * 10
    ) %>%
    ggplot() +
    aes(x = year, y = value, color = runtype) +
    geom_line() +
    facet_grid(vars(name), vars(casename), scales = "free_y") +
    cowplot::theme_cowplot() +
    theme(axis.text.x = element_text(angle = 90),
          axis.title = element_blank()),
  default_median_fluxes_png = ggsave(
    file_out("analysis/figures/default-median-fluxes.png"),
    default_median_fluxes_gg,
    width = 10.1, height = 7
  ),
  default_median_fluxes_knit = knitr::include_graphics(file_in(
    "analysis/figures/default-median-fluxes.png"
  ))
))

plan <- plan <- bind_plans(plan, drake_plan(
  both_long_p = both_results %>%
    select(casename, runtype, pft_py) %>%
    unnest(pft_py) %>%
    select(-c(case:param_id)) %>%
    mutate(pft = set_pft(pft)) %>%
    pivot_longer(-c(casename:datetime, pft)) %>%
    filter(month(datetime) %in% 6:8) %>%
    annual_mean(),
  default_median_lai_gg = both_long_p %>%
    filter(name == "mmean_lai_py") %>%
    ggplot() +
    aes(x = year, y = value, color = pft, group = pft) +
    geom_line() +
    facet_grid(vars(runtype), vars(casename)) +
    scale_color_manual(values = pfts("color")) +
    labs(y = "LAI") +
    cowplot::theme_cowplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90)
    ),
  default_median_lai_png = ggsave(
    file_out("analysis/figures/default-median-lai.png"),
    default_median_lai_gg,
    width = 10.1, height = 7
  ),
  default_median_lai_knit = knitr::include_graphics(file_in(
    "analysis/figures/default-median-lai.png"
  ))
))

stop()

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

