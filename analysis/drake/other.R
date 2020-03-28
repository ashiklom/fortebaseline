plan <- bind_plans(plan, drake_plan(
  umbs_map_gg = {
    umbs <- sf::st_sfc(sf::st_point(c(-84.6975, 45.5625)), crs = 4326)
    states <- rnaturalearth::ne_states(returnclass = "sf")
    ggplot(states) +
      geom_sf() +
      geom_sf(data = umbs, size = 3) +
      coord_sf(xlim = c(-90, -80), ylim = c(40, 48)) +
      theme_bw()
  },
  umbs_map_png = ggsave(
    file_out("analysis/figures/umbs-map.png"),
    umbs_map_gg,
    width = 6, height = 6.5, dpi = 300
  )
))
