# Read various input UMBS data
library(tidyverse)
library(readxl)
library(fs)

data_dir <- path("analysis", "data", "retrieved")

# UMBS Ameriflux LAI data
# 60 m plots: A2:E20
# All plots (but not permanent 13): G2:J20
# 13 permanent plots: L2:T20
ameriflux_lai_file <- path(data_dir, "Ameriflux LAI trends, 1997-2014.xlsx")
ameriflux_lai <- list(
  "60 m" = read_xlsx(ameriflux_lai_file, range = "A3:E20",
                     col_names = c("year", "aplusb", "other",
                                   "total", "ratio")),
  "All but 13" = read_xlsx(ameriflux_lai_file, range = "G3:J20",
                           col_names = c("aplusb", "other", "total", "ratio")),
  "13 permanent" = read_xlsx(ameriflux_lai_file, range = "L3:T20",
                             col_names = c("aplusb", "var1", "95ci1",
                                           "other", "var2", "95ci2",
                                           "total", "var", "95ci"))
) %>%
  map(~mutate(.x, year = read_xlsx(ameriflux_lai_file, range = "A3:A20",
                                   col_names = "year")[[1]])) %>%
  bind_rows(.id = "plots")

ameriflux_lai_summary <- ameriflux_lai %>%
  select(year, plots, lai = total) %>%
  summarize(
    mean_lai = mean(lai, na.rm = TRUE),
    sd_lai = sd(lai, na.rm = TRUE),
    n_lai = n()
  )

# UMBS Aboveground Woody NPP
umbs_npp_file <- path(data_dir, "UMB&UMd_AboveWoodNPP.xlsx")
umbs_npp <- read_xlsx(umbs_npp_file, range = "A3:C21",
                      col_names = c("year", "npp_wood", "sd"))
umbs_npp_summary <- umbs_npp %>%
  summarize(
    mean_npp_wood = mean(npp_wood),
    sd_npp_wood = mean(sd)
  ) %>%
  mutate_all(`/`, 1000) # Convert from kgC to MgC (ha-1 yr-1)

# NPP_LAI data
npp_lai_file <- path(data_dir, "NPP_LAI_All AF Litter Traps .xlsx")
