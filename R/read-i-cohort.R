#' Read cohort data from `I` file
#'
#' @param fname File name
#' @return `tibble` of cohort data
#' @author Alexey Shiklomanov
#' @export
read_i_cohort <- function(fname) {
  datetime <- fname %>%
    fs::path_file() %>%
    stringr::str_extract(paste("[[:digit:]]{4}",
                               "[[:digit:]]{2}",
                               "[[:digit:]]{2}",
                               "[[:digit:]]{6}",
                               sep = "-")) %>%
    lubridate::ymd_hms(tz = "UTC")

  stopifnot(lubridate::is.POSIXct(datetime))

  workflow_run <- fname %>%
    stringr::str_match("PEcAn_([[:digit:]]+)/out/([[:digit:]]+)/") %>%
    .[, -1] %>%
    as.numeric()

  common <- list(
    workflow_id = workflow_run[[1]],
    run_id = workflow_run[[2]],
    datetime = datetime
  )

  vars <- cohort_vars()
  hf <- hdf5r::H5File$new(fname)
  on.exit(hf$close_all(), add = TRUE)
  cohort_data <- purrr::map(vars, hf_var_get, hf = hf) %>%
    setNames(tolower(vars)) %>%
    purrr::discard(is.null)

  # Radiation profile gets special treatment
  rad_profile <- hf_var_get(hf, "FMEAN_RAD_PROFILE_CO") %>%
    t() %>%
    `colnames<-`(c("par_beam_down", "par_beam_up",
                   "par_diff_down", "par_diff_up",
                   "nir_beam_down", "nir_beam_up",
                   "nir_diff_down", "nir_diff_up",
                   "tir_diff_down", "tir_diff_up")) %>%
    tibble::as_tibble()
 
  tibble::tibble(!!!common, !!!cohort_data, !!!rad_profile)
}


get_same_dims_as <- function(nc, var) {
  ncout <- capture.output(print(nc))
  dim_names <- nc[[c("var", var, "dim")]] %>%
    purrr::map_chr("name")
  ncout %>%
    stringr::str_match(sprintf("([[:graph:]]+)\\[%s\\]",
                               paste(dim_names, sep = ","))) %>%
    .[, 2] %>%
    na.omit() %>%
    as.character()
}


hf_var_get <- function(hf, var) {
  tryCatch(
    hf[[var]]$read(),
    error = function(e) {
      warning("Failed to read variable `", var, "` with error: ",
              conditionMessage(e))
      return(NULL)
    }
  )
}
