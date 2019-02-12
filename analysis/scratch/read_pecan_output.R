library(tidyverse)
library(furrr)
library(fst)

plan(multiprocess)

workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
runs <- list.files(outdir)

# Get a sense of how many runs finished
finished <- tibble(
  files = list.files(outdir, "[[:digit:]]{4}.nc", recursive = TRUE)
) %>%
  separate(files, c("run", "year", "ext")) %>%
  mutate(year = as.numeric(year))

finished_ct <- finished %>%
  count(run) %>%
  print(n = Inf)

read_nc_time <- function(nc, varname = "time") {
  raw_unit <- ncdf4::ncatt_get(nc, varname, "units")[["value"]]
  rxp <- "([[:alpha:]]+?)s? since ([[:digit:]-]+ [[:digit:]:]+)"
  unit <- gsub(rxp, "\\1", raw_unit)
  dfun <- switch(
    unit,
    day = lubridate::ddays,
    hour = lubridate::dhours,
    second = lubridate::dseconds
  )
  base_date <- lubridate::ymd_hms(raw_unit)
  value <- base_date + dfun(ncdf4::ncvar_get(nc, "time"))
  value
}

read_ncfile <- function(file) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  tibble::tibble(
    time = read_nc_time(nc),
    npp = ncdf4::ncvar_get(nc, "NPP"),
    gpp = ncdf4::ncvar_get(nc, "GPP"),
    ## Rt = ncdf4::ncvar_get(nc, "TotalResp"),
    ## Rh = ncdf4::ncvar_get(nc, "HeteroResp"),
    ## trans = ncdf4::ncvar_get(nc, "TVeg"),
    lai = ncdf4::ncvar_get(nc, "LAI"),
    watertable = ncdf4::ncvar_get(nc, "WaterTableD")
    ## rootmoist = ncdf4::ncvar_get(nc, "RootMoist"),
    ## surf_runoff = ncdf4::ncvar_get(nc, "Qs"),
  )
}

read_member <- function(rundir, pb = NULL) {
  if (!is.null(pb)) pb$tick()
  run_id <- basename(rundir)
  all_nc_files <- list.files(rundir, "[[:digit:]]{4}\\.nc", full.names = TRUE)
  ed_results <- purrr::map_dfr(all_nc_files, read_ncfile)
  ed_results %>%
    tidyr::gather(variable, value, -time) %>%
    dplyr::mutate(run_id = !!run_id)
}

## pb <- progress::progress_bar$new(total = NROW(finished_ct))
ed_results_long <- future_map_dfr(
  file.path(outdir, finished_ct[["run"]]),
  read_member,
  .progress = TRUE
)

fst::write_fst(ed_results_long, "analysis/data/derived-data/ed-ensemble-out.fst")
