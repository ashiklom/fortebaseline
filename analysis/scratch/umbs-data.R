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

##################################################
data_dir <- path("analysis", "data", "retrieved")
data_dir2 <- path(data_dir, "umbs-ameriflux")

proc_dat <- function(f) {
  read_csv(
    f,
    comment = "#",
    col_types = cols(
      TIMESTAMP_START = col_datetime(format = "%Y%m%d%H%M"),
      TIMESTAMP_END = col_datetime(format = "%Y%m%d%H%M"),
      .default = col_double()
    )) %>%
  mutate_if(is_double, ~na_if(.x, -9999))
  ## filter_at(vars(-starts_with("TIMESTAMP")), any_vars(!is.na(.)))
}

umbs_data <- dir_ls(data_dir2, regexp = "AMF_.*\\.csv") %>%
  map(proc_dat)

names(umbs_data) <- names(umbs_data) %>%
  path_file() %>%
  str_remove("^AMF_US-") %>%
  str_remove("\\.csv$")

umbs_df <- bind_rows(umbs_data, .id = "site")

umbs_annual <- umbs_df %>%
  group_by(site, year = lubridate::year(TIMESTAMP_START)) %>%
  summarize(NEE = mean(NEE_PI_F, na.rm = TRUE)) %>%
  ungroup() %>%
  # Unit conversion
  mutate(NEE = udunits2::ud.convert(NEE * 12.011, "ug m-2 s-1", "Mg ha-1 year-1"))

ggplot(umbs_annual) +
  aes(x = year, y = NEE, color = site) +
  geom_line()


um3 <- path(data_dir2, "AMF_US-UM3_BASE_HH_1-5.csv") %>%

um3 %>%
  mutate(NEE = if_else(is.na(NEE_PI), NEE_PI_F, NEE_PI)) %>%
  ggplot() +
  aes(x = TIMESTAMP_START, y = NEE) +
  geom_line()
