plan <- bind_plans(plan, drake_plan(
  use_params = tribble(
    ~param_id, ~label, ~why,
    399, "A", "MH-dominated in 1999, high NPP",
    39,  "B", "LH-dominated in 1999, high NPP",
    362, "C", "High competition in 1999, high NPP",
    284, "D", "EH-dominated in 1999, high NPP",
    114, "E", "Pine-dominated in 1999, high NPP"
  ),
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
      legend.position = "bottom"
    ),
  lai_pft_plot_png = ggsave(
    file_in("analysis/figures/lai-pft.png"),
    lai_pft_plot_gg,
    width = 9.7, height = 6.1
  ),
  lai_pft_plot_knit = knitr::include_graphics(file_in(
    "analysis/figures/lai-pft.png"
  ))
))

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
