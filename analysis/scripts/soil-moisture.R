library(fs)
library(readr)
library(readxl)
library(dplyr, mask.ok = c("filter", "lag", "intersect", "setdiff",
                           "setequal", "union"))
library(purrr)
library(tidyr)
library(ggplot2)
library(here)

ameriflux_dir <- "~/Projects/forte_project/umbs-data/ameriflux/"
stopifnot(dir_exists(ameriflux_dir))

umbs_dist_flux_file <- path(ameriflux_dir, "AMF_US-UMd_BASE_HH_7-5.csv")

amf_cols <- cols(
  TIMESTAMP_START = col_datetime("%Y%m%d%H%M"),
  TIMESTAMP_END = col_datetime("%Y%m%d%H%M"),
  .default = col_number()
)
umbs_dist_flux <- read_csv(umbs_dist_flux_file, skip = 2, col_types = amf_cols,
                           na = "-9999")

# From Chris Gough, who got them from Valeriy Ivanov
umbs_dist_soil_depths <- c(0, 5, 15, 30, 60, 100, 200, 300) / 100 # cm -> m

umbs_dist_sm <- umbs_dist_flux %>%
  select(tstart = TIMESTAMP_START, tend = TIMESTAMP_END,
         starts_with("SWC"), -matches("_PI_")) %>%
  filter_at(vars(starts_with("SWC")), any_vars(!is.na(.))) %>%
  gather(variable, SWC, starts_with("SWC")) %>%
  separate(variable, c("variable", "h", "v", "r")) %>%
  select(-variable, -h, -r) %>%
  mutate(depth = umbs_dist_soil_depths[as.integer(v)])

# Need to calculate ED2 "soil moisture index", which is a linear scale
# between wilting point (0), field capacity (1), and saturation (2)
soil_params <- PEcAn.data.land::soil_params(sand = 0.92, clay = 0.01)
wp <- soil_params[["volume_fraction_of_condensed_water_in_soil_at_wilting_point"]]
fc <- soil_params[["volume_fraction_of_water_in_soil_at_field_capacity"]]

umbs_dist_sm_summary <- umbs_dist_sm %>%
  filter(lubridate::month(tstart) %in% 6:9,
         depth > 0) %>% # Remove depth 0 
  group_by(depth) %>%
  summarize(SWC = mean(SWC, na.rm = TRUE)) %>%
  mutate(slmstr = (SWC/100 - wp) / (fc - wp))

write_csv(umbs_dist_sm_summary,
          here("analysis", "data", "derived-data", "soil-moisture.csv"))

if (interactive()) {
  ggplot(umbs_dist_sm_summary) +
    aes(x = depth, y = slmstr) +
    geom_line() +
    ylab(expression("Soil moisture index (0-1)")) +
    xlab(expression("Soil depth (m)")) +
    scale_x_reverse() +
    coord_flip() +
    theme_bw()
}
