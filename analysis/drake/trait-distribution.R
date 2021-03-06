### Download input files
plan <- bind_plans(plan, drake_plan(
  run_params_dl = target(
    download.file(osf_url(params_osf), file_out(!!ensemble_params_file)),
    trigger = trigger(change = get_timestamp(params_osf),
                      condition = !file.exists(ensemble_params_file)),
    hpc = FALSE
  ),
  trait_distribution_dl = target(
    download.file(osf_url(td_osf), file_out(!!trait_distribution_file)),
    trigger = trigger(change = get_timestamp(td_osf),
                      condition = !file.exists(trait_distribution_file)),
    hpc = FALSE
  )
))

### Trait distribution figures
plan <- bind_plans(plan, drake_plan(
  ed2_param_table = read_csv(
    file_in("analysis/data/derived-data/parameter-table.csv"),
    col_types = "ffcccccc",
    comment = "#"
  ),
  trait_distribution = readRDS(file_in(!!trait_distribution_file)) %>%
    mutate(pft = factor(pft, pfts("pft")),
           trait = factor(trait, ed2_param_table[["ED Name"]])) %>%
    left_join(ed2_param_table, c("trait" = "ED Name")),
  ed2_default_params = ed_default_params() %>%
    mutate(trait = factor(trait, ed2_param_table[["ED Name"]])) %>%
    semi_join(trait_distribution, c("pft", "trait")) %>%
    left_join(ed2_param_table, c("trait" = "ED Name")),
  ed2_default_plot = ed2_default_params %>%
    mutate(
      default_value = if_else(trait == "water_conductance",
                              log10(default_value),
                              default_value),
      `Display name` = fct_recode(`Display name`, "log10(Water cond.)" = "Water cond.")
    ),
  param_dist_data = trait_distribution %>%
    unnest(draws) %>%
    mutate(
      draws = if_else(trait == "water_conductance",
                      log10(draws),
                      draws),
      Median = if_else(trait == "water_conductance",
                       log10(Median),
                       Median),
      `Display name` = fct_recode(`Display name`,
                                  "log10(Water cond.)" = "Water cond.")
    ),
  param_dist_gg = ggplot(param_dist_data) +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin(alpha = 0.5) +
    geom_point(aes(y = default_value,
                   color = "ED-2.2 default", shape = "ED-2.2 default"),
               data = ed2_default_plot, size = 2) +
    geom_point(aes(y = Median,
                   color = "Posterior median", shape = "Posterior median"),
               size = 2) +
    # Parameter values
    facet_wrap(vars(`Display name`), scales = "free_y") +
    scale_fill_manual(values = pfts("color")) +
    scale_color_manual(values = c("ED-2.2 default" = "red1",
                                  "Posterior median" = "blue1")) +
    scale_shape_manual(values = c("ED-2.2 default" = 3,
                                  "Posterior median" = 4)) +
    guides(color = guide_legend(title = "Parameter", order = 2,
                                direction = "horizontal"),
           shape = guide_legend(title = "Parameter", order = 2,
                                direction = "horizontal"),
           fill = guide_legend(override.aes = list(size  = 0),
                               direction = "horizontal",
                               order = 1)) +
    labs(x = "PFT", fill = "PFT") +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = c(0.4, 0.1),
          legend.box = "vertical"),
  param_dist_png = ggsave(
    file_out(!!path(fig_dir, "param-dist.png")),
    param_dist_gg,
    width = 15.3, height = 9.9, dpi = 300
  )
))
