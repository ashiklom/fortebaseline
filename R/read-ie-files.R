#' Read observed (-O-) file
#'
#' @param fname File name
#' @param .pb Optional progress bar object
#' @return
#' @author Alexey Shiklomanov
#' @export
read_o_file <- function(fname, .pb = NULL) {
  if (!is.null(.pb)) .pb$tick()
  hf <- ncdf4::nc_open(fname)
  on.exit(ncdf4::nc_close(hf))
  common <- get_common(fname)

  cohort_vars <- fortebaseline:::cohort_vars()
  cohort_data <- purrr::map(cohort_vars, ncdf4::ncvar_get, nc = hf) %>%
    setNames(tolower(cohort_vars)) %>%
    purrr::discard(is.null)
  rad_profile <- ncdf4::ncvar_get(hf, "FMEAN_RAD_PROFILE_CO") %>%
    t() %>%
    `colnames<-`(c("par_beam_down", "par_beam_up", "par_diff_down",
                   "par_diff_up", "nir_beam_down", "nir_beam_up", "nir_diff_down",
                   "nir_diff_up", "tir_diff_down", "tir_diff_up")) %>%
    tibble::as_tibble()
  cohort_out <- tibble::tibble(!!!common, !!!cohort_data, !!!rad_profile)

  scalar_vars <- fortebaseline:::scalar_vars()
  scalar_data <- purrr::map(scalar_vars, ncdf4::ncvar_get, nc = hf) %>%
    setNames(tolower(scalar_vars)) %>%
    purrr::discard(is.null)
  scalar_out <- tibble::tibble(!!!common, !!!scalar_data)

  soil_vars <- fortebaseline:::soil_vars()
  soil_data <- purrr::map(soil_vars, ncdf4::ncvar_get, nc = hf) %>%
    setNames(tolower(soil_vars)) %>%
    purrr::discard(is.null)
  soil_out <- tibble::tibble(!!!common, slz = ncdf4::ncvar_get(hf, "SLZ"), !!!soil_data)

  py_vars <- fortebaseline:::py_vars()
  py_data <- purrr::map(py_vars, ncdf4::ncvar_get, nc = hf,
                        start = c(6, 1, 1), count = c(6, -1, -1)) %>%
    purrr::map(~rowSums(.x[c(4:6, 1), ])) %>%
    setNames(tolower(py_vars)) %>%
    purrr::discard(is.null)
  py_out <- tibble::tibble(!!!common, pft = c(9:11, 6), !!!py_data)

  list(
    scalar = scalar_out,
    cohort = cohort_out,
    soil = soil_out,
    pft = py_out
  )
}

#' Read monthly (-E-) file
#'
#' @param fname File name
#' @param .pb Optional progress bar object
#' @return
#' @author Alexey Shiklomanov
#' @export
read_e_file <- function(fname, .pb = NULL) {
  hf <- ncdf4::nc_open(fname)
  on.exit(ncdf4::nc_close(hf))
  common <- get_common(fname)

  ncget <- purrr::possibly(ncdf4::ncvar_get, NULL)

  cohort_vars <- fortebaseline:::cohort_vars_m()
  if ("AGB_CO" %in% names(hf$var)) {
    cohort_data <- purrr::map(cohort_vars, ncget, nc = hf) %>%
      setNames(tolower(cohort_vars)) %>%
      purrr::discard(is.null)
    rad_profile <- ncdf4::ncvar_get(hf, "MMEAN_RAD_PROFILE_CO") %>%
      t() %>%
      `colnames<-`(c("par_beam_down", "par_beam_up", "par_diff_down",
                     "par_diff_up", "nir_beam_down", "nir_beam_up", "nir_diff_down",
                     "nir_diff_up", "tir_diff_down", "tir_diff_up")) %>%
      tibble::as_tibble()
    cohort_out <- tibble::tibble(!!!common, !!!cohort_data, !!!rad_profile)
  } else {
    warning("File ", fname, " has no cohort data!")
    cohort_out <- NULL
  }

  scalar_vars <- fortebaseline:::scalar_vars_m()
  scalar_data <- purrr::map(scalar_vars, ncget, nc = hf) %>%
    setNames(tolower(scalar_vars)) %>%
    purrr::discard(is.null)
  scalar_out <- tibble::tibble(!!!common, !!!scalar_data)

  soil_vars <- fortebaseline:::soil_vars_m()
  soil_data <- purrr::map(soil_vars, ncget, nc = hf) %>%
    setNames(tolower(soil_vars)) %>%
    purrr::discard(is.null)
  soil_out <- tibble::tibble(!!!common, slz = ncdf4::ncvar_get(hf, "SLZ"), !!!soil_data)

  py_vars <- fortebaseline:::pft_vars_m()
  py_data <- purrr::map(py_vars, ncget, nc = hf,
                        start = c(6, 1, 1), count = c(6, -1, -1)) %>%
    purrr::map(~rowSums(.x[c(4:6, 1), ])) %>%
    setNames(tolower(py_vars)) %>%
    purrr::discard(is.null)
  py_out <- tibble::tibble(!!!common, pft = c(9:11, 6), !!!py_data)

  list(
    scalar = scalar_out,
    cohort = cohort_out,
    soil = soil_out,
    pft = py_out
  )

}

#' Read directory full of E files
#'
#' @param outdir Output directory containing E files
#' @param overwrite (Logical) If `TRUE`, ignore current saved RDS file.
#' @param save (Logical) If `TRUE`, save result to RDS file for faster loading later.
#' @return Nested `data.frame` containing the output directory and scalar,
#'   cohort, soil, and PFT results
#' @author Alexey Shiklomanov
#' @export
read_efile_dir <- function(outdir, overwrite = FALSE, save = TRUE) {
  out_rds <- fs::path(outdir, "monthly-output.rds")
  if (!overwrite && file.exists(out_rds)) {
    message("Loading cached output")
    result_dfs <- readRDS(out_rds)
    return(result_dfs)
  }
  message("Reading all HDF5 files")
  efiles <- fs::dir_ls(outdir, glob = "*/*-E-*")
  e_data_list <- furrr::future_map(efiles, read_e_file, .progress = TRUE)

  result_dfs <- tibble::tibble(
    basename = fs::path_file(outdir),
    scalar = list(purrr::map_dfr(e_data_list, "scalar")),
    cohort = list(purrr::map_dfr(e_data_list, "cohort")),
    soil = list(purrr::map_dfr(e_data_list, "soil")),
    pft = list(purrr::map_dfr(e_data_list, "pft")),
    outdir = outdir
  )
  if (save) saveRDS(result_dfs, out_rds)
  result_dfs
}

get_common <- function(fname) {
  case <- basename(dirname(fname))
  model_id <- substring(case, 4, 6)
  param_id <- as.numeric(substring(case, 0, 3))
  list(
    case = case,
    model_id = model_id,
    param_id = param_id,
    datetime = get_datetime(fname)
  )
}

get_datetime <- function(fname) {
  datestring <- fname %>%
    fs::path_file() %>%
    stringr::str_extract(paste("[[:digit:]]{4}",
                               "[[:digit:]]{2}", "[[:digit:]]{2}", "[[:digit:]]{6}",
                               sep = "-"))
  year <- as.numeric(substring(datestring, 1, 4))
  month <- as.numeric(substring(datestring, 6, 7))
  day <- as.numeric(substring(datestring, 9, 10))
  # To accommodate monthly files
  if (day == 0) day <- 1
  hr <- as.numeric(substring(datestring, 12, 13))
  min <- as.numeric(substring(datestring, 14, 15))
  sec <- as.numeric(substring(datestring, 16, 17))
  ISOdatetime(year, month, day, hr, min, sec, tz = "UTC")
}
