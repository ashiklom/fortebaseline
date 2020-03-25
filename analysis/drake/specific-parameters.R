plan <- bind_plans(plan, drake_plan(
  fit_observed_params = fit_observed %>%
    distinct(param_id) %>%
    mutate(label = toupper(letters[seq_len(n())])),
  fit_observed_param_values = params %>%
    inner_join(fit_observed_params, "param_id") %>%
    inner_join(pfts(), c("name" = "bety_name")) %>%
    select(-name, -shortname, -num, -color) %>%
    pivot_longer(-c(param_id, label, pft),
                 names_to = "trait",
                 values_to = "value"),
  lai_pft_plot_gg = pft_data %>%
    mutate(param_id = as.numeric(substr(case, 0, 3)),
           model_id = substr(case, 4,6)) %>%
    semi_join(fit_observed_params, "param_id") %>%
    left_join(fit_observed_params, "param_id") %>%
    left_join(models, "model_id") %>%
    left_join(fit_observed, c("model", "param_id")) %>%
    mutate(fit_obs = !is.na(category)) %>%
    ggplot() +
    aes(x = year, y = mmean_lai_py, color = pft,
        linetype = fit_obs, size = fit_obs) +
    geom_line() +
    facet_grid(
      vars(label),
      vars(fct_relabel(model, ~gsub(" ", "\n", .x))),
      scales = "free_y"
    ) +
    labs(y = "Leaf area index", color = "PFT",
         linetype = "Fits observed", size = "Fits observed") +
    scale_color_manual(values = pfts("color")) +
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "3111")) +
    scale_size_manual(values = c("TRUE" = 1, "FALSE" = 0.6)) +
    coord_cartesian(ylim = c(0, 10)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text.y = element_text(angle = 0)
    ),
  lai_pft_plot_png = ggsave(
    file_out("analysis/figures/lai-pft.png"),
    lai_pft_plot_gg,
    width = 9.7, height = 6.1, dpi = 300
  )
))
