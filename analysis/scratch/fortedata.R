## remotes::install_github("FoRTExperiment/fortedata")

library(fortedata)

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

inv <- fd_inventory() %>%
  inner_join(species_pft_map, "Species")

inv_summary <- inv %>%
  group_by(PFT) %>%
  summarize(basal_area = sum(DBH_cm ^ 2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(f_area = basal_area / sum(basal_area)) %>%
  arrange(desc(f_area))

1 / sum(inv_summary$f_area^2)
