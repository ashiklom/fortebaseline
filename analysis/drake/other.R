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

plan <- bind_plans(plan, drake_plan(
  forte_inv_summary = {
    species_pft_map <- tribble(
      ~Species, ~PFT,
      "POGR", "Early",
      "FAGR", "Late",
      "ACSA", "Late",
      "QURU", "Mid",
      "ACPE", "Mid",
      "ACRU", "Mid",
      "BEPA", "Early",
      "????", "Other",
      "BEAL", "Other",
      "AMEL", "Other",
      "TSCA", "Other",
      "PIST", "Pine",
      "PIRE", "Other",
      "ADRU", "Other",
      "QUR", "Mid",
      "POTR", "Early"
    )

    inv <- fortedata::fd_inventory() %>%
      inner_join(species_pft_map, "Species")

    inv %>%
      group_by(shortname = PFT) %>%
      summarize(basal_area = sum(DBH_cm ^ 2, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        f_area = basal_area / sum(basal_area),
        lai = obs_lai$mean * f_area,
        pft = factor(shortname, pfts("shortname"), pfts("pft"))
      ) %>%
      arrange(desc(f_area))
  }
))
