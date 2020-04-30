fix_cases <- function(x) {
  x2 <- gsub("^.*-", "", x)
  mod_levels <- c("CTS", "CTP", "CMS", "CMP",
                  "FTS", "FTP", "FMS", "FMP")
  factor(x2, mod_levels)
}

### Download files from OSF
plan <- bind_plans(plan, drake_plan(
  default_results_dl = target(
    download.file(osf_url(default_results_osf),
                  file_out(!!default_results_file)),
    trigger = trigger(change = get_timestamp(default_results_osf),
                      condition = !file.exists(default_results_file)),
    hpc = FALSE
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
    pivot_longer(c(npp, lai), names_to = "variable"),
  structure_compare_default_gg = ggplot(structure_default_data) +
    aes(x = year, y = value, linetype = crown, color = traits, group = model) +
    geom_line() +
    geom_pointrange(
      aes(x = 1999, y = mean, ymin = low, ymax = hi),
      data = observations %>%
        semi_join(structure_default_data, "variable") %>%
        left_join(models, "model"),
      color = "black",
      show.legend = FALSE
    ) +
    labs(linetype = "Crown model", color = "Trait plasticity") +
    facet_grid(
      vars(variable = factor(variable, c("npp", "lai"))), vars(rtm),
      scales = "free_y",
      switch = "y",
      labeller = labeller(
        variable = as_labeller(c(
          "npp" = "atop(NPP, (MgC ~ ha^-1 ~ yr^-1))",
          "lai" = "LAI"
        ), default = label_parsed),
        rtm = function(labels) paste0("Canopy RTM: ", labels)
      )
    ) +
    scale_color_manual(values = c("#F5793A", "#0F2080")) +
    theme_bw() +
    theme(
      text = element_text(size = 14),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.background = element_blank(),
      strip.placement = "outside"
    ),
  structure_compare_default_png = ggsave(
    file_out("analysis/figures/structure-compare-default.png"),
    structure_compare_default_gg,
    width = 6.5, height = 4.2, dpi = 300
  )
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
  )
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

plan <- bind_plans(plan, drake_plan(
  median_results_dl = target(
    download.file(osf_url(median_results_osf), file_out(!!median_results_file)),
    trigger = trigger(change = get_timestamp(median_results_osf),
                      condition = !file.exists(median_results_file))
  ),
  median_results = file_in(!!median_results_file) %>%
    readRDS() %>%
    mutate(
      runtype = "median",
      casename = fix_cases(casename)
    ),
  both_results = bind_rows(default_results, median_results),
  both_long_s = both_results %>%
    select(casename, runtype, scalar) %>%
    mutate(runtype = factor(runtype, c("default", "median"),
                       c("ED-2.2 default", "Posterior median"))) %>%
    unnest(scalar) %>%
    select(-c(case:param_id), -age, -area, -area_si,
           -latitude, -longitude, -starts_with("mmsq")) %>%
    pivot_longer(-c(casename:datetime)) %>%
    annual_mean(),
  default_median_fluxes_gg = both_long_s %>%
    filter(grepl("mmean_(gpp|npp|plresp)_py", name)) %>%
    mutate(
      variable = stringr::str_remove(name, "mmean_") %>%
        stringr::str_remove("_py") %>%
        factor(c("gpp", "plresp", "npp"), c("GPP", "RA", "NPP")),
      # Unit conversion
      value = value * 10
    ) %>%
    select(-name) %>%
    left_join(models, c("casename" = "model_id")) %>%
    ggplot() +
    aes(x = year, y = value, color = runtype) +
    geom_line() +
    geom_pointrange(
      aes(x = 1999, y = mean, ymin = low, ymax = hi),
      data = observations %>%
        filter(variable == "npp") %>%
        mutate(variable = factor(variable, c("gpp", "plresp", "npp"),
                                 c("GPP", "RA", "NPP"))),
      color = "black",
      show.legend = FALSE
    ) +
    facet_grid(
      vars(variable), vars(model),
      scales = "free_y",
      labeller = labeller(
        .default = label_value,
        model = function(labels) gsub(" ", "\n", labels)
      ),
      switch = "y"
    ) +
    scale_color_manual(values = c("#F5793A", "#0F2080")) +
    labs(y = expression("Flux" ~ ("MgC" ~ "ha"^-1 ~ "year"^-1)),
         color = "Parameters") +
    theme_bw() +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.placement = "outside"),
  default_median_fluxes_png = ggsave(
    file_out("analysis/figures/default-median-fluxes.png"),
    default_median_fluxes_gg,
    width = 10.1, height = 7, dpi = 300
  )
))

plan <- bind_plans(plan, drake_plan(
  both_long_p = both_results %>%
    select(casename, runtype, pft_py) %>%
    unnest(pft_py) %>%
    select(-c(case:param_id)) %>%
    mutate(pft = set_pft(pft)) %>%
    pivot_longer(-c(casename:datetime, pft)) %>%
    filter(month(datetime) %in% 6:8) %>%
    annual_mean(),
  default_median_lai_data = both_long_p %>%
    filter(name == "mmean_lai_py") %>%
    left_join(models, c("casename" = "model_id")),
  default_median_lai_gg = {
    obs2 <- tidyr::crossing(
      forte_inv_summary,
      runtype = unique(default_median_lai_data$runtype),
      model = levels(default_median_lai_data$model)
    ) %>%
      rename(value = lai) %>%
      mutate(year = 2000)

    ggplot(default_median_lai_data) +
        aes(x = year, y = value, color = pft, group = pft) +
        geom_line() +
        geom_point(data = obs2) +
        facet_grid(
          vars(runtype), vars(model),
          labeller = labeller(
            .default = label_value,
            model = function(labels) gsub(" ", "\n", labels)
          )
        ) +
        scale_color_manual(values = pfts("color")) +
        labs(y = "LAI", color = "Plant functional type") +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme_bw() +
        theme(
          text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom",
          strip.background = element_blank()
        )
    },
  default_median_lai_png = ggsave(
    file_out("analysis/figures/default-median-lai.png"),
    default_median_lai_gg,
    width = 10.1, height = 7
  )
))

# Aboveground biomass distribution figure
plan <- bind_plans(plan, drake_plan(
  default_agb_distribution_data = default_results %>%
    select(runtype, casename, cohort) %>%
    unnest(cohort) %>%
    group_by(casename, pft, year = year(datetime), datetime) %>%
    # Sum over PFTs
    summarize(agb = sum(agb_co, na.rm = TRUE)) %>%
    # Average over years
    summarize(agb = mean(agb, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(casename, year) %>%
    mutate(f_agb = agb / sum(agb)) %>%
    ungroup() %>%
    mutate(pft = set_pft(pft)) %>%
    left_join(models, c("casename" = "model_id")),
  default_agb_distribution_gg = ggplot(default_agb_distribution_data) +
    aes(x = year, y = f_agb, fill = pft) +
    geom_area() +
    facet_wrap(vars(model), ncol = 4,
               labeller = labeller(model = label_wrap_gen(10))) +
    scale_fill_manual(values = pfts("color")) +
    theme_bw() +
    theme(text = element_text(size = 14),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom") +
    labs(fill = "PFT",
         y = expression("Aboveground biomass fraction")),
  default_agb_distribution_png = ggsave(
    file_out("analysis/figures/default-agb-distribution.png"),
    default_agb_distribution_gg,
    width = 6.5, height = 4.2, dpi = 300
  )
))
