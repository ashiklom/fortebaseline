### PFT plot
plan <- bind_plans(plan, drake_plan(
  pft_jitterplot_gg = pft_data %>%
    filter(year %in% c(1910, 1920, 1950, 1980)) %>%
    mutate(year = factor(year),
           model_id = substr(case, 4, 6)) %>%
    left_join(models, "model_id") %>%
    ggplot() +
    aes(x = year, y = agb_frac, color = pft) +
    geom_jitter(size = 0.5, alpha = 0.2, position = position_jitterdodge()) +
    facet_wrap(vars(model), ncol = 2) +
    scale_color_manual(values = pfts("color")) +
    labs(y = "AGB fraction", color = "PFT") +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
    theme_cowplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      strip.background = element_blank(),
      legend.position = "bottom"
    ),
  pft_jitterplot_png = ggsave(
    file_out("analysis/figures/pft-jitterplot.png"),
    pft_jitterplot_gg,
    width = 6.6, height = 6.4
  ),
  pft_jitterplot_knit = knitr::include_graphics(file_in(
    "analysis/figures/pft-jitterplot.png"
  ))
))

### PFT ternary plot
plan <- bind_plans(plan, drake_plan(
  tern_data = pft_data %>%
    mutate(model_id = substr(case, 4, 6)) %>%
    filter(year %in% c(1920, 1950, 1980)) %>%
    select(case, model_id, year, pft, agb_frac) %>%
    pivot_wider(names_from = "pft", values_from = "agb_frac") %>%
    mutate(`Mid/Late` = `Mid hardwood` + `Late hardwood`) %>%
    select(-`Mid hardwood`, -`Late hardwood`) %>%
    rename("Early" = "Early hardwood") %>%
    left_join(models, "model_id"),
  pft_tern_gg = ggplot(tern_data) +
    aes(x = Early, y = `Mid/Late`, z = Pine) +
    ggtern::geom_tri_tern(bins = 10) +
    facet_grid(
      vars(year),
      vars(fct_relabel(model, ~gsub(" ", "\n", .)))
    ) +
    scale_fill_viridis_c(na.value = "white") +
    guides(fill = guide_colorbar(title = "Count")) +
    ggtern::coord_tern() +
    theme(
      axis.title = element_blank()
    ),
  pft_tern_png = ggsave(
    file_out("analysis/figures/pft-tern.png"),
    pft_tern_gg,
    width = 10, height = 4
  ),
  pft_tern_knit = knitr::include_graphics(file_in(
    "analysis/figures/pft-tern.png"
  ))
))

stop()