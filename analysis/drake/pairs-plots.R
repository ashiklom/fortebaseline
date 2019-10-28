### Pairs plots
npp_lai_pairs <- function(dat, year, labs) {
  dat_sub <- dat %>%
    filter(!is.na(model), year == !!year)
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
    ggtitle(year) +
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
    npp_lai_pairs(both_wide, .year, use_params),
    transform = map(.year = c(1925, 1999))
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

### STOP HERE
stop()

both_wide %>%
  filter(year == 1920) %>%
  select(model_id, starts_with("mmean"), `Early hardwood`:`Pine`) %>%
  GGally::ggpairs(
    .,
    aes(color = model_id),
    size = 0.5,
    alpha = 0.2,
    columns = names(.)[-1]
  )

tern_data %>%
  filter(year == 1920, model_id == "FMS") %>%
  ggplot() +
  aes(x = Early, y = `Mid/Late`, z = Pine) +
  ggtern::geom_tri_tern(bins = 10) +
  scale_fill_viridis_c(na.value = "white") +
  ggtern::coord_tern()

get_density <- function(x, y, z, ...) {
  dens <- misc3d::kde3d(x, y, z, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  iz <- findInterval(z, dens$z)
  ii <- cbind(ix, iy, iz)
  dens$d[ii]
}

pft_tern_pl

tern_data2 <- tern_data %>%
  group_by(model_id) %>%
  mutate(dens = get_density(`Early hardwood`, `ML`, `Pine`)) %>%
  ungroup()
ggplot(tern_data2) +
  aes(x = `Early hardwood`, y = `ML`, z = `Pine`, color = dens) +
  geom_point() +
  facet_wrap(vars(model_id), ncol = 2) +
  scale_color_viridis_c() +
  ggtern::coord_tern()


pft_data %>%
  mutate(model_id = substr(case, 4, 6)) %>%
  left_join(models, "model_id") %>%
  ggplot() +
  aes(x = year, y = agb_frac, group = case, color = color) +
  geom_line(alpha = 0.2, size = 0.2) +
  facet_grid(vars(pft), vars(fct_relabel(model, ~gsub(" ", "\n", .)))) +
  labs(y = "AGB fraction") +
  scale_color_identity() +
  theme_cowplot() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    strip.background = element_blank()
  )
