plan <- bind_plans(plan, drake_plan(
  use_params = tribble(
    ~param_id, ~label, ~why,
    399, "A", "MH-dominated in 1999, high NPP",
    39,  "B", "LH-dominated in 1999, high NPP",
    362, "C", "High competition in 1999, high NPP",
    284, "D", "EH-dominated in 1999, high NPP",
    114, "E", "Pine-dominated in 1999, high NPP"
  ),
  use_param_values = params %>%
    inner_join(use_params, "param_id") %>%
    inner_join(pfts(), c("name" = "bety_name")) %>%
    select(-why, -name, -shortname, -num, -color) %>%
    ## filter(label %in% c("C", "D")) %>%
    pivot_longer(-c(param_id, label, pft),
                 names_to = "trait",
                 values_to = "value"),
  lai_pft_plot_gg = pft_data %>%
    mutate(param_id = as.numeric(substr(case, 0, 3))) %>%
    inner_join(use_params, "param_id") %>%
    inner_join(cases, c("case", "param_id")) %>%
    ggplot() +
    aes(x = year, y = mmean_lai_py, color = pft) +
    geom_line() +
    facet_grid(
      vars(label),
      vars(fct_relabel(model, ~gsub(" ", "\n", .x))),
      scales = "free_y"
    ) +
    labs(y = "Leaf area index", color = "PFT") +
    scale_color_manual(values = pfts("color")) +
    theme_cowplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      legend.position = "bottom",
      strip.background = element_blank()
    ),
  lai_pft_plot_png = ggsave(
    file_out("analysis/figures/lai-pft.png"),
    lai_pft_plot_gg,
    width = 9.7, height = 6.1
  )
))
