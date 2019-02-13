library(tidyverse)
library(data.table)
library(furrr)

workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
runs <- list.files(outdir)

get_monthly_lai <- function(rundir, year) {
  filename <- file.path(
    rundir,
    glue::glue("analysis-T-{year}-00-00-000000-g01.h5")
  )
  nc <- ncdf4::nc_open(filename)
  lai_raw <- ncdf4::ncvar_get(
    nc,
    "LAI_PY",
    start = c(6, 1, 1, 1),
    count = c(6, -1, -1, -1) # Only include PFTs 6, (7), 8, 9, 10, 11
  )[-2,,] # Drop PFT 7

  pft_levels <- c("Early hardwood", "Mid hardwood", "Late hardwood",
                  "Pine", "Late conifer")
  pftnames <- factor(pft_levels[c(4, 5, 1:3)], pft_levels)
  basedate <- ISOdate(year, 1, 1, 0, 0, 0, "UTC")

  data.table::melt(lai_raw) %>%
    tibble::as_tibble() %>%
    dplyr::rename(ipft = Var1, cohort = Var2, timestep = Var3) %>%
    dplyr::mutate(
      pft = pftnames[ipft],
      dt = basedate + lubridate::dhours((timestep - 1) / 2),
      month = lubridate::floor_date(dt, "month")
    ) %>%
    dplyr::group_by(month, pft, cohort) %>%
    dplyr::summarize(lai = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(month, pft) %>%
    dplyr::summarize(lai = sum(lai)) %>%
    dplyr::ungroup()
}

get_ensemble_lai <- function(rundir, years = seq(1902, 1990)) {
  ensemble <- basename(rundir)
  future_map_dfr(years, get_monthly_lai, rundir = rundir) %>%
    dplyr::mutate(ensemble = !!ensemble)
}

plan(list(tweak(multiprocess, workers = 2),
          tweak(multiprocess, workers = 4)))
all_lai <- future_map_dfr(file.path(outdir, runs), possibly(get_ensemble_lai, NULL),
                          .progress = TRUE)

fst::write_fst(all_lai, "analysis/data/derived-data/ed-lai-output.fst")
