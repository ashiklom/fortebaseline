lai_pft_plot <- function(dat, label) {
    ggplot(dat) +
    aes(x = year, y = mmean_lai_py, color = pft,
        linetype = fit_obs, size = fit_obs) +
    geom_line() +
    facet_grid(
      vars(label),
      vars(fct_relabel(model, ~gsub(" ", "\n", .x))),
      scales = "free_y"
    ) +
    labs(y = "Leaf area index", color = "PFT",
         linetype = label, size = label) +
    scale_color_manual(values = pfts("color")) +
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "3111")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 0.6)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

plan <- bind_plans(plan, drake_plan(
  fit_observed_params = fit_observed %>%
    distinct(param_id) %>%
    slice(c(5, 2, 1, 4, 3, 6)) %>%
    mutate(label = toupper(head(letters, n()))),
  fit_observed_param_values = params %>%
    inner_join(fit_observed_params, "param_id") %>%
    inner_join(pfts(), c("name" = "bety_name")) %>%
    select(-name, -shortname, -num, -color) %>%
    pivot_longer(-c(param_id, label, pft),
                 names_to = "trait",
                 values_to = "value"),
  lai_pft_fitobs_data = pft_data %>%
    mutate(param_id = as.numeric(substr(case, 0, 3)),
           model_id = substr(case, 4,6)) %>%
    semi_join(fit_observed_params, "param_id") %>%
    left_join(fit_observed_params, "param_id") %>%
    left_join(models, "model_id") %>%
    left_join(fit_observed, c("model", "param_id")) %>%
    mutate(fit_obs = !is.na(category)),
  lai_pft_plot_fitobs_gg = lai_pft_plot(lai_pft_fitobs_data, "Fit observed") +
    coord_cartesian(ylim = c(0, 8)),
  lai_pft_plot_fitobs_png = ggsave(
    file_out("analysis/figures/lai-pft-fitobs.png"),
    lai_pft_plot_fitobs_gg,
    width = 9.7, height = 6.1, dpi = 300
  )
))

plan <- bind_plans(plan, drake_plan(
  high_diversity_params = high_diversity %>%
    inner_join(last_ten, c("param_id", "model")) %>%
    arrange(desc(mmean_lai_py)) %>%
    filter(mmean_lai_py > 1) %>%
    distinct(param_id) %>%
    mutate(label = toupper(tail(letters, n()))),
  high_diversity_param_values = params %>%
    inner_join(high_diversity_params, "param_id") %>%
    inner_join(pfts(), c("name" = "bety_name")) %>%
    select(-name, -shortname, -num, -color) %>%
    pivot_longer(-c(param_id, label, pft),
                 names_to = "trait",
                 values_to = "value"),
  lai_pft_diverse_data = pft_data %>%
    mutate(param_id = as.numeric(substr(case, 0, 3)),
           model_id = substr(case, 4,6)) %>%
    semi_join(high_diversity_params, "param_id") %>%
    left_join(high_diversity_params, "param_id") %>%
    left_join(models, "model_id") %>%
    left_join(high_diversity, c("model", "param_id")) %>%
    mutate(fit_obs = !is.na(category)),
  lai_pft_plot_diverse_gg = lai_pft_plot(lai_pft_diverse_data,
                                         expression(N["PFT, eff"] > 2)) +
    coord_cartesian(ylim = c(0, 7)),
  lai_pft_plot_diverse_png = ggsave(
    file_out("analysis/figures/lai-pft-diverse.png"),
    lai_pft_plot_diverse_gg,
    width = 9.7, height = 6.1, dpi = 300
  )
))

scatter_pie <- function(indat, filterdat, filterdat_values,
                        obslist, ...) {
  pltdat <- indat %>%
    semi_join(filterdat, "param_id") %>%
    left_join(distinct(filterdat_values, param_id, label),
              "param_id") %>%
    mutate(x = as.numeric(model) + (runif(n(), -0.2, 0.2)),
           pid = as.numeric(factor(param_id)))

  colorvec <- pfts("color")
  names(colorvec) <- pfts("pft")

  ggplot(pltdat) +
    scatterpie::geom_scatterpie(
      aes(x = x, y = mmean_npp_py, group = pid),
      cols = as.character(pfts("pft")),
      data = pltdat,
      ...
    ) +
    geom_label_repel(
      aes(x = x, y = mmean_npp_py, group = pid, label = label)
    ) +
    geom_hline(yintercept = c(obslist$lo, obslist$hi), linetype = "dashed") +
    scale_x_continuous(
      breaks = seq_along(levels(pltdat$model)),
      labels = gsub(" ", "\n", levels(pltdat$model))
    ) +
    scale_fill_manual(
      breaks = pfts("pft"),
      values = colorvec
    ) +
    labs(y = expression(NPP ~ (MgC ~ ha^-1)),
         fill = "PFT") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom")
}


plan <- bind_plans(plan, drake_plan(
  fitobs_pie = scatter_pie(
    last_ten,
    fit_observed,
    fit_observed_param_values,
    obs_npp
  ),
  fitobs_pie_png = ggsave(
    file_out("analysis/figures/pie-fitobs.png"),
    fitobs_pie,
    width = 6.2, height = 5.0, dpi = 300
  ),
  diversity_pie = scatter_pie(
    last_ten,
    high_diversity,
    high_diversity_param_values,
    obs_npp
  ),
  diversity_pie_png = ggsave(
    file_out("analysis/figures/pie-diversity.png"),
    diversity_pie,
    width = 6.2, height = 5.0, dpi = 300
  )
))
