library(fs)
library(readr)
library(readxl)
library(dplyr, mask.ok = c("filter", "lag", "intersect", "setdiff",
                           "setequal", "union"))
library(purrr)
library(tidyr)
library(ggplot2)
library(here)

ameriflux_path <- "~/Projects/forte_project/umbs-data/ameriflux/"

umbs_dist_flux_file <- path(ameriflux_path, "AMF_US-UMd_BASE_HH_7-5.csv")

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

umbs_dist_sm_summary <- umbs_dist_sm %>%
  filter(lubridate::month(tstart) %in% 6:9,
         depth > 0) %>% # Remove depth 0 
  group_by(depth) %>%
  summarize(SWC = mean(SWC, na.rm = TRUE))

write_csv(umbs_dist_sm_summary,
          here("analysis", "data", "derived-data", "soil-moisture.csv"))

if (interactive()) {
  ggplot(umbs_dist_sm_summary) +
    aes(x = depth, y = SWC) +
    geom_line() +
    ylab(expression("Soil water content (fraction)")) +
    xlab(expression("Soil depth (m)")) +
    scale_x_reverse() +
    coord_flip() +
    theme_bw()
}

## umbs_dist_bif_file <- path(ameriflux_path, "AMF_US-UMd_BIF_LATEST.xlsx")
## umbs_other_bif_file <- path(ameriflux_path, "AMF_US-UMB_BIF_LATEST.xlsx")

## umbs_dist_bif <- read_xlsx(umbs_dist_bif_file)
## umbs_dist_bif %>%
##   filter(grepl("SOIL", VARIABLE)) %>%
##   pull(DATAVALUE)
