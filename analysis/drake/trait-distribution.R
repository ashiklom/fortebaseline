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
  trait_distribution = readRDS(file_in(!!trait_distribution_file)) %>%
    mutate(pft = factor(pft, pfts("pft"))),
  ed2_default_params = ed_default_params() %>%
    semi_join(trait_distribution, c("pft", "trait")),
  param_dist_gg = trait_distribution %>%
    unnest(draws) %>%
    ggplot() +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin(alpha = 0.5) +
    geom_point(aes(y = default_value, color = "default", shape = "default"),
               data = ed2_default_params, size = 2) +
    geom_point(aes(y = Median, color = "median", shape = "median"),
               data = trait_distribution, size = 2) +
    # Parameter values
    ## geom_text(aes(y = value, label = label), data = use_param_values) +
    facet_wrap(vars(trait), scales = "free_y") +
    scale_fill_manual(values = pfts("color")) +
    scale_color_manual(values = c("default" = "red1", "median" = "blue1")) +
    scale_shape_manual(values = c("default" = 3, "median" = 4)) +
    guides(color = guide_legend(title = "param"),
           shape = guide_legend(title = "param"),
           fill = guide_legend(override.aes = list(size  = 0))) +
    labs(x = "PFT", fill = "PFT") +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          legend.position = c(0.7, 0.1),
          legend.box = "horizontal"),
  param_dist_png = ggsave(
    file_out(!!path(fig_dir, "param-dist.png")),
    param_dist_gg,
    width = 15.3, height = 9.9
  ),
  param_dist_knit = knitr::include_graphics(file_in(!!path(
    fig_dir, "param-dist.png"))),
  ))
