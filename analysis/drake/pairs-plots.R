### Pairs plots
npp_lai_pairs <- function(dat, start_year, end_year, labs) {
  dat_sub <- dat %>%
    filter(!is.na(model), year >= !!start_year, year <= !!end_year) %>%
    group_by(param_id, model, color) %>%
    summarize_at(vars(mmean_lai_py, mmean_npp_py), mean) %>%
    ungroup()
  labs2 <- labs %>%
    inner_join(dat_sub, "param_id") %>%
    distinct(param_id, model, label, mmean_lai_py, mmean_npp_py)

  ggplot(dat_sub) +
    aes(x = mmean_lai_py, y = mmean_npp_py) +
    geom_point(aes(color = color), size = 0.7, alpha = 0.7) +
    geom_smooth(method = mgcv::gam, color = "gray30") +
    geom_hline(yintercept = c(6, 7), linetype = "dashed") +
    geom_vline(xintercept = 3.97 + c(-1, 1) * 1.96 * 0.423,
               linetype = "dashed") +
    geom_point(color = "black", data = labs2) +
    geom_text_repel(aes(label = label), data = labs2,
                    min.segment.length = 0) +
    scale_color_identity() +
    facet_wrap(vars(model), ncol = 2) +
    labs(x = "Total LAI", y = expression(NPP ~ (MgC ~ ha^-1 ~ year^-1))) +
    ggtitle(sprintf("%d - %d", start_year, end_year)) +
    theme_cowplot() +
    theme(strip.text = element_text(size = 10))
}

plan <- bind_plans(plan, drake_plan(
  pft_wide = pft_data %>%
    mutate(model_id = substr(case, 4, 6)) %>%
    select(case, pft, year, model_id, agb_frac) %>%
    setDT() %>%
    dcast(case + year + model_id ~ pft, value.var = "agb_frac") %>%
    as_tibble(),
  lai_wide = pft_totals %>%
    filter(variable == "mmean_lai_py") %>%
    select(case, year, mmean_lai_py = value),
  both_wide = scalar_data %>%
    setDT() %>%
    dcast(case + year ~ variable, value.var = "value") %>%
    as_tibble() %>%
    left_join(pft_wide, c("case", "year")) %>%
    left_join(lai_wide, c("case", "year")) %>%
    left_join(
      cases %>%
        select(case, model_id, param_id),
      c("case", "model_id")
    ) %>%
    left_join(models, c("model_id")),
  npp_lai_pairs_yr = target(
    npp_lai_pairs(both_wide, .ayear, .zyear, use_params),
    transform = map(.ayear = c(1920, 1975),
                    .zyear = c(1950, 1999))
  ),
  npp_lai_pairs_gg = target(
    plot_grid(npp_lai_pairs_yr, nrow = 1),
    transform = combine(npp_lai_pairs_yr)
  ),
  npp_lai_pairs_png = ggsave(
    file_out("analysis/figures/npp-lai-pairs.png"),
    npp_lai_pairs_gg,
    width = 14, height = 8
  ),
  npp_lai_pairs_knit = knitr::include_graphics(file_in(
    "analysis/figures/npp-lai-pairs.png"
  ))
))
