### PFT plot
plan <- bind_plans(plan, drake_plan(
  pft_histogram = pft_data %>%
    filter(year %in% c(1910, 1920, 1950, 1980)) %>%
    mutate(
      year = factor(year),
      model_id = substr(case, 4, 6),
      agb_frac_bin = cut(agb_frac, c(-Inf, 0, 0.25, 0.5, 0.75, 0.999, 1),
                         labels = c("0", "(0,0.25]", "(0.25, 0.5]", "(0.5, 0.75]",
                                    "(0.75, 1)", "1"))
    ) %>%
    group_by(pft, year, model_id, agb_frac_bin) %>%
    summarize(nagb_frac = n()) %>%
    ungroup() %>%
    left_join(models, "model_id") %>%
    ggplot() +
    aes(x = agb_frac_bin, y = nagb_frac, fill = pft) +
    geom_col(position = "dodge") +
    facet_grid(vars(year), vars(model),
               labeller = labeller(model = label_wrap_gen(10))) +
    labs(x = "AGB Fraction", y = "Count", fill = "PFT") +
    scale_fill_manual(values = pfts("color")) +
    theme_bw() +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.background = element_blank(),
      legend.position = "bottom"
    ),
  pft_histogram_png = ggsave(
    file_out("analysis/figures/pft-histogram.png"),
    pft_histogram,
    width = 10.8, height = 7.5
  )
))

### Effective number of PFTs
plan <- bind_plans(plan, drake_plan(
  pft_n_effective = pft_data %>%
    group_by(case, year) %>%
    summarize(
      simpson = sum(agb_frac ^ 2),
      d2 = 1 / simpson
    ) %>%
    ungroup() %>%
    pivot_longer(
      c(simpson, d2),
      names_to = "variable",
      values_to = "value"
    )
))
