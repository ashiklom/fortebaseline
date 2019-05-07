library(fs)
library(here)
library(magrittr, include.only = "%>%")
library(readr)
library(stringr)
library(dplyr, mask.ok = c("filter", "lag", "intersect", "setdiff",
                           "setequal", "union"))
library(hdf5r)
library(lubridate, exclude = c("here", "intersect", "setdiff", "union"),
        mask.ok = c("as.difftime", "date"))
library(purrr, exclude = "flatten_df")
library(tidyr)

requireNamespace("PEcAn.ED2", quietly = TRUE)

local_dir <- here("analysis", "data", "retrieved")
local_in_dir <- path(local_dir, "met-cruncep") %>% dir_create()
local_out_dir <- path(local_dir, "met-custom") %>% dir_create()

# Copy all the files from in to out
.cp <- dir_copy(local_in_dir, local_out_dir, overwrite = TRUE)

# Read all the met files in as a tidy data.frame
met_ts <- tibble(
  full_path = dir_ls(local_in_dir, glob = "*.h5"),
  fname = path_file(full_path),
  date = str_remove(fname, "\\.h5$") %>% parse_date_time("ym")
) %>% arrange(date)

# Read the Mauna Loa and Law Dome records
law_dome_url <- "https://cdiac.ess-dive.lbl.gov/ftp/trends/co2/lawdome.smoothed.yr20"
law_dome_raw <- readLines(law_dome_url)
law_dome_cols <- c("mean_air_age", "CO2_20yr_smooth")
law_dome_data <- read_table(law_dome_raw, skip = 23, col_names = law_dome_cols)

mlo_url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
mlo_cols <- c("year", "month", "decimal_year", "average", "interpolated", "trend", "no_days")
mlo_data <- read_table(mlo_url, skip = 72, col_names = mlo_cols) %>%
  mutate_at(vars(average, interpolated), ~if_else(. < 0, NA_real_, .))

# Combine them both
co2_record <- law_dome_data %>%
  rename(year = mean_air_age, law_dome = CO2_20yr_smooth) %>%
  left_join(
    mlo_data %>%
      group_by(year) %>%
      summarize(mlo = mean(interpolated, na.rm = TRUE)),
    by = "year"
  ) %>%
  transmute(
    year = year,
    co2 = map2_dbl(law_dome, mlo, lift_vd(mean, na.rm = TRUE))
  )

co2_record_6hr <- tibble(
  date = seq(min(met_ts[["date"]]), max(met_ts[["date"]]), by = "6 hours"),
  co2 = approx(co2_record$year, co2_record$co2, decimal_date(date))[["y"]]
) %>%
  mutate(date_floor = floor_date(date, "month"))

if (interactive()) {
  plot(co2 ~ date, data = co2_record_6hr, type = "l")
}

co2_record_nested <- co2_record_6hr %>%
  group_by(date_floor) %>%
  nest() %>%
  right_join(met_ts, by = c("date_floor" = "date"))

add_co2 <- function(file, data) {
  co2 <- data[["co2"]]
  co2_array <- array(co2, c(length(co2), 1, 1))
  hf <- H5File$new(file)
  hf[["co2"]] <- co2_array
  hf$close_all()
  invisible(TRUE)
}

co2_wrote <- co2_record_nested %>%
  mutate(wrote = map2_lgl(full_path, data, possibly(add_co2, FALSE)))

# Write out the revised ED met header
emh <- PEcAn.ED2::read_ed_metheader(path(local_out_dir, "ED_MET_DRIVER_HEADER"),
                                    check = FALSE, check_files = FALSE)
emh[[1]][["variables"]] <- emh[[1]][["variables"]] %>%
  filter(variable != "co2") %>%
  add_row(variable = "co2", flag = 1, update_frequency = 21600)

PEcAn.ED2::write_ed_metheader(emh, path(local_out_dir, "ED_MET_DRIVER_HEADER"))
