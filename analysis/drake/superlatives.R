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
    ## arrange(desc(npft_eff)) %>%
    ## top_n(10) %>%
    filter(npft_eff > 2) %>%
    mutate(category = "Most diverse") %>%
    select(param_id, model, category),
  superlatives = bind_rows(fit_observed, high_diversity)
))

model_super_plot <- function(model, all_params, super_params,
                             model_id = model) {
  super_sub <- super_params %>%
    filter(model == {{model}})
  gg <- ggplot(all_params) +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin(aes(color = pft), alpha = 0.5) +
    geom_point(aes(y = value, shape = category),
               position = position_dodge(width = 0.3),
               data = super_sub) +
    # Parameter values
    facet_wrap(vars(`Display name`), scales = "free_y") +
    scale_fill_manual(values = pfts("color")) +
    scale_color_manual(values = pfts("color")) +
    scale_shape_manual(values = c("Fit observations" = 19, "Most diverse" = 4)) +
    guides(fill = guide_legend(override.aes = list(size  = 0),
                               direction = "horizontal",
                               order = 1),
           color = FALSE) +
    labs(title = model, x = "PFT", fill = "PFT") +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.4, 0.1),
          legend.box = "vertical")
  outfile <- sprintf("analysis/figures/super-param-%s.png", model_id)
  ggsave(outfile, gg, width = 15.3, height = 9.9, dpi = 300)
  outfile
}

plan <- bind_plans(plan, drake_plan(
  params_wide_super = params_wide %>%
    mutate_at(vars(ends_with("water_conductance")), log10) %>%
    inner_join(superlatives, "param_id"),
  params_super = params_wide_super %>%
    pivot_longer(-c(param_id, model, category),
                 names_to = "variable",
                 values_to = "value") %>%
    extract(variable, c("PFT", "trait"), "(.*?)\\.(.*)") %>%
    mutate(pft = factor(PFT, pfts("shortname"), pfts("pft"))) %>%
    left_join(ed2_param_table, c("trait" = "ED Name")) %>%
    mutate(
      `Display name` = fct_recode(`Display name`,
                                  "log10(Water cond.)" = "Water cond.")
    ),
  params_super_png = target(
    model_super_plot(models[["model"]], param_dist_data, params_super,
                     models[["model_id"]]),
    target = "file",
    dynamic = map(models)
  )
))
