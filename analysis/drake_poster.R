if (!exists("plan")) {
  stop("This script is not meant to be run standalone. ",
       "Rather, it should be `source`d as part of `drake.R`.")
}

model_short <- function(x) {
  y <- x
  ## y <- gsub("finite", "fin.", y)
  ## y <- gsub("closed", "clo.", y)
  y <- gsub("two-stream", "2S", y)
  y <- gsub("multi-scatter", "MS", y)
  y
}

poster_plan <- drake_plan(
  model_scale_n = setNames(model_scale, gsub("\\.", "\n", names(model_scale))),
  poster_mainfig_gg = monthly_means_site %>%
    filter(month %in% 7:9) %>%
    tidyr::gather(variable, value, NPP, LAI, AGB) %>%
    mutate(variable = fct_inorder(variable)) %>%
    # Aggregate runs
    group_by(crown, rtm, traits, variable, year) %>%
    summarize(
      mean = mean(value),
      lo = quantile(value, 0.1),
      hi = quantile(value, 0.9)
    ) %>%
    filter(!is.na(variable)) %>%
    ungroup() %>%
    mutate(model = interaction(crown, rtm, traits, sep = "\n") %>%
             factor(names(model_scale_n))) %>%
    ggplot() +
    aes(x = year, y = mean, ymin = lo, ymax = hi, fill = model) +
    geom_line() +
    geom_ribbon(alpha = 0.7) +
    facet_grid(vars(variable), vars(model),
               scales = "free_y", switch = "y") +
    guides(fill = FALSE) +
    scale_fill_manual(values = model_scale_n) +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text.y = element_blank(),
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.background.y = element_blank()),
  poster_mainfig = ggsave(plot = poster_mainfig_gg,
                          filename = file_out("analysis/figures/poster_mainfig.png"),
                          width = 9.75, height = 7),
  poster_params_labels = tribble(
    ~trait, ~pretty,
    "SLA", "SLA",
    "Vcmax", "Vcmax",
    "quantum_efficiency", "Quantum eff.",
    "stomatal_slope", "Stomatal slope",
    "cuticular_cond", "Cuticular cond.",
    "r_fract", "Reprod. C Frac.",
    "f_labile", "Labile C frac.",
    "nonlocal_dispersal", "Dispersal",
    "fineroot2leaf", "Root:Leaf C",
    "Vm_low_temp", "Cold stress temp.",
    "water_conductance", "Water cond.",
    "mort2", "Mortality C bal.",
    "seedling_mortality", "Seedling mort.",
    "growth_resp_factor", "Growth resp.",
    "leaf_respiration_rate_m2", "Leaf resp.",
    "leaf_turnover_rate", "Leaf turnover",
    "root_respiration_rate", "Root resp.",
    "root_turnover_rate", "Root turnover",
    "c2n_fineroot", "C:N root",
    "c2n_leaf", "C:N leaf",
    "leaf_width", "Leaf width"
  ),
  poster_params_gg = ma_posterior %>%
    inner_join(poster_params_labels, by = "trait") %>%
    mutate(pft = factor(pft, pfts("pft")),
           pretty = fct_inorder(pretty)) %>%
    unnest(draws) %>%
    filter(draws < quantile(draws, 0.975),
           draws > quantile(draws, 0.025)) %>%
    ggplot() +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin() +
    ## aes(x = draws, fill = pft) +
    ## geom_density() +
    facet_wrap(vars(pretty), scales = "free", strip.position = "left") +
    scale_fill_viridis_d() +
    labs(x = "PFT", fill = "PFT") +
    theme_cowplot() +
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside"),
  poster_params = ggsave(plot = poster_params_gg,
                         filename = file_out("analysis/figures/poster_params.png"),
                         width = 10.2, height = 6.3),
  poster_bigfacet_gg = monthly_means %>%
    ungroup() %>%
    select(crown, rtm, traits, date, pft, runs,
           mmean_lai_py) %>%
    filter(month(date) == 7) %>%
    inner_join(
      # Order runs in order of increasing max Pine LAI
      monthly_means %>%
        ungroup() %>%
        select(workflow_id, runs, pft, mmean_lai_py) %>%
        filter(pft == "Pine") %>%
        group_by(workflow_id, runs) %>%
        summarize(pine_lai = max(mmean_lai_py)) %>%
        mutate(run_i = rank(pine_lai)) %>%
        # Restrict to a few different trajectories
        filter(run_i %in% c(1, 3, 6, 10)),
      by = "runs") %>%
    mutate(model = interaction(crown, rtm, traits, sep = "\n") %>%
             factor(names(model_scale_n))) %>%
    ggplot() +
    aes(x = date, y = mmean_lai_py, color = pft) +
    geom_line(size = 1) +
    facet_grid(vars(run_i), vars(model), drop = TRUE) +
    labs(y = "Leaf area index", color = "PFT") +
    scale_color_viridis_d() +
    guides(color = FALSE) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    theme_cowplot() +
    theme(strip.text.y = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x = element_blank(),
          strip.background = element_blank()),
  poster_bigfacet_png = ggsave(plot = poster_bigfacet_gg,
                               filename = file_out("analysis/figures/poster_facet.png"),
                               width = 10.3, height = 7.5),
  poster_barplot_gg = both_uncertainty %>%
    gather(variable, variance, GPP:AGB) %>%
    filter(variable %in% c("AGB", "LAI", "NPP")) %>%
    mutate(variable = factor(variable, c("NPP", "LAI", "AGB"))) %>%
    ggplot() +
    aes(x = model, y = variance, fill = model) +
    geom_col() +
    facet_wrap(vars(variable), scales = "free_y") +
    scale_fill_manual(values = model_scale) +
    guides(fill = FALSE) +
    labs(x = "Model structure", y = "Prediction variance") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()),
  poster_barplot_png = ggsave(plot = poster_barplot_gg,
                              filename = file_out("analysis/figures/poster_barplot.png"),
                              width = 10.3, height = 3.9),
  poster_sensitivity_plot_piece = target(
    sensitivity_plot_data %>%
      inner_join(poster_params_labels, by = "trait") %>%
      filter(yvar %in% c("AGB", "LAI", "NPP")) %>%
      mutate(yvar = factor(yvar, c("NPP", "LAI", "AGB")),
             model = factor(model, names(model_scale_n)) %>%
               fct_relabel(model_short)) %>%
      ggplot() +
      aes(x = pft, y = fct_rev(pretty), fill = YYY) +
      geom_tile() +
      facet_grid(vars(yvar), vars(model)) +
      SCALE +
      labs(x = "PFT", fill = LAB) +
      theme_cowplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                       size = rel(0.7)),
            axis.text.y = element_text(size = rel(0.6)),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.key.width = grid::unit(0.1, "npc"),
            strip.background.x = element_blank(),
            strip.text.x = element_text(size = rel(0.8))) +
      MOD,
    transform = map(YYY = c(elasticity, pvar),
                    LAB = c("Elasticity", "Partial variance"),
                    SCALE = c(scale_fill_gradient2(),
                              scale_fill_continuous(low = "white", high = "blue")),
                    MOD = c(theme(strip.text.y = element_blank()),
                            theme()))
  ),
  poster_sensitivity_plot_gg = target(
    cowplot::plot_grid(poster_sensitivity_plot_piece),
    transform = combine(poster_sensitivity_plot_piece)
  ),
  poster_sensitivity_plot_png = ggsave(
    plot = poster_sensitivity_plot_gg,
    filename = file_out("analysis/figures/poster_sensitivity.png"),
    width = 10.3, height = 10.3
  )
)

plan <- bind_plans(plan, poster_plan)
