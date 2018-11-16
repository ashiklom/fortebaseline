if (FALSE) {
  filename <- "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000032/out/99000000030/analysis-T-1932-00-00-000000-g01.h5"
  filename <- "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000032/out/99000000030/analysis-Y-1932-00-00-000000-g01.h5"
}

#' Read ED output files
#'
#' Read the different kinds of ED output file (see Details).
#'
#' ED can produce one of five kinds of output file:
#' - Daily averages (D)
#' - Monthly averages (M)
#' - Yearly averages (Y)
#' - Instantaneous fluxes, one file per year (T, for "tower")
#' - Monthly means of the diurnal cycle (Q) -- number of points is `1/FRQANL`
#' - History files (all variables needed for restart) (S) -- Restart
#' interval determined by `FRQHIS`
#' - "Fast" analysis files (F) -- Polygon-level averages at a custom
#' frequency (`FRQANL`)
#' - Observation output files (O) -- Same as F files, but only at
#' timesteps specified by `OBSTIME_DB`

read_ed_t_file <- function(filename) {
  hf <- ncdf4::nc_open(filename)
  dim_lengths <- purrr::map_int(hf[["dim"]], "len")
  udim_lengths <- unique(dim_lengths)
  # Number of time steps is always the first dimension
  ntime <- udim_lengths[1]
  # Second dimension is number of patches (?)
  npatch <- udim_lengths[2]
  # Third dimension is soil depth
  nsoil <- udim_lengths[3]
  # Fourth dimension is only for LAI, and refers to each cohort
  ncohort <- udim_lengths[4]
  # Fifth dimension is number of PFTs, usually 17
  npft <- udim_lengths[5]

  # Pull out variables based on dimensions
  var_names <- names(dim_lengths)
  ts_vars <- strip_dimid(var_names[dim_lengths == ntime])
  patch_vars <- strip_dimid(var_names[dim_lengths == npatch])
  soil_vars <- strip_dimid(var_names[dim_lengths == nsoil])
  cohort_vars <- strip_dimid(var_names[dim_lengths == ncohort])
  pft_vars <- strip_dimid(var_names[dim_lengths == npft])
  ts_patch_diff <- setdiff(ts_vars, patch_vars) == 0

  # In T files, there are three kinds of variables:
  # - Univariate time series -- ntime x npatch
  # - Soil time series -- ntime x npatch x nsoil
  # - Cohort x PFT time series -- ntime x npatch x ncohort x pft
  ncdf4::nc_close(hf)
}

strip_dimid <- function(var_name) {
  gsub("_[[:digit:]]+$", "", var_name)
}
