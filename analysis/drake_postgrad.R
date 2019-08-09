if (!exists("plan")) {
  stop("This script is not meant to be run standalone. ",
       "Rather, it should be `source`d as part of `drake.R`.")
}

postgrad_plan <- drake_plan(
  trait_dict = read_csv(file_in("analysis/data/derived-data/parameter-table.csv")),
  pg_trait_data = trait_distribution %>%
    left_join(trait_dict, c("trait" = "ED Name")) %>%
    mutate(pft = factor(pft, pfts("pft")),
           trait = factor(trait, trait_dict[["ED Name"]]),
           `Display name` = factor(`Display name`,
                                   trait_dict[["Display name"]])) %>%
    unnest(draws) %>%
    filter(draws < quantile(draws, 0.975),
           draws > quantile(draws, 0.025)),
  pg_param_dist_gg = ggplot(pg_trait_data) +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin() +
    facet_wrap(vars(`Display name`), scales = "free_y") +
    scale_fill_manual(values = pfts("color")) +
    labs(x = "PFT", fill = "PFT") +
    theme_cowplot() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom"),
  pg_param_dist_png = ggsave2(file_out("analysis/figures/postgrad_params.png"),
                              pg_param_dist_gg,
                              height = 10, width = 13),
  pg_jja_long = jja_long %>%
    filter(variable %in% c("npp", "lai")),
  pg_jja_summary = jja_summary %>%
    filter(variable %in% c("npp", "lai")),
  pg_summary_ts_plot = ggplot(pg_jja_long) +
    aes(x = year) +
    geom_line(aes(y = value, group = param_id, color = color), alpha = 0.25) +
    geom_line(data = pg_jja_summary, aes(y = hi),
              color = "black", linetype = "dashed") +
    geom_line(data = pg_jja_summary, aes(y = lo),
              color = "black", linetype = "dashed") +
    geom_line(aes(y = avg), color = "black", size = 1, data = pg_jja_summary) +
    geom_pointrange(data = observations,
                    aes(y = mean, ymin = low, ymax = hi),
                    color = "black") +
    scale_color_identity() +
    facet_grid(vars(variable), vars(model), scales = "free_y",
               switch = "y", labeller = my_labeller) +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          strip.placement = "outside",
          strip.background = element_blank()),
  pg_summary_ts_plot_png = ggsave2(
    file_out("analysis/figures/postgrad_summary.png"),
    pg_summary_ts_plot,
    width = 11.58,
    height = 8.29
  ),
  pg_pairs_time_averaged = time_averages %>%
    ungroup() %>%
    ggplot() +
    aes(x = lai, y = npp, color = model) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_point(data = observations_wide,
               aes(x = lai_mean, y = npp_mean),
               size = 3,
               shape = 17,
               color = "black",
               inherit.aes = FALSE) +
    geom_rect(data = observations_wide,
              aes(xmin = lai_low, xmax = lai_hi,
                  ymin = npp_low, ymax = npp_hi),
              fill = NA,
              linetype = "dashed",
              color = "black",
              inherit.aes = FALSE) +
    scale_color_manual(values = model_colors) +
    coord_cartesian(xlim = c(0, 9), ylim = c(0, 11)) +
    labs(x = "LAI", y = expression(NPP ~ (MgC ~ ha^-1 ~ year ^ -1))) +
    guides(color = FALSE) +
    facet_wrap(vars(model), scales = "fixed", ncol = 2) +
    theme_cowplot() +
    theme(legend.position = "bottom",
          strip.background = element_blank()),
  pg_pairs_time_averaged_png = ggsave(
    file_out("analysis/figures/postgrad_subset.png"),
    pg_pairs_time_averaged,
    width = 7.9, height = 8.3
  ),
  pg_pairs_labelled = pg_pairs_time_averaged +
    geom_text_repel(data = my_subsets_time_avg,
                    aes(label = label),
                    color = "black",
                    min.segment.length = 0),
  pg_pairs_labelled_png = ggsave(
    file_out("analysis/figures/postgrad_params_labs.png"),
    pg_pairs_labelled,
    width = 7.9, height = 8.3
  ),
  pg_lai_pft_plot = lai_q90 %>%
    inner_join(models, by = "model_id") %>%
    mutate(param_id = as.numeric(substring(case, 0, 3))) %>%
    inner_join(my_subsets, "param_id") %>%
    ggplot() +
    aes(x = year, y = lai, color = pft) +
    geom_line() +
    facet_grid(vars(label), vars(model), labeller = my_labeller) +
    scale_color_manual(values = pfts("color")) +
    labs(y = "Leaf area index", color = "PFT") +
    theme_cowplot() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      strip.background.x = element_blank(),
      strip.text.y = element_text(angle = 0),
      legend.position = "bottom"
    ),
  pg_lai_pft_plot_png = ggsave(
    file_out("analysis/figures/postgrad_lai_pft.png"),
    pg_lai_pft_plot,
    width = 9.8, height = 8.3
  )
)

plan <- bind_plans(plan, postgrad_plan)
