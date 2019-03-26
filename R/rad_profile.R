#' Read the radiation profile from a history file and return as a tidy
#' `data.frame`
#'
#' @param file Full path to history file
#' @return `data.frame` with columns `workflow_id`, `datetime`,
#'   `cohort` (integer - tallest first), and one column per radiation flux.
#' @author Alexey Shiklomanov
#' @export
read_tidy_rad_profile <- function(file) {
  nc <- ncdf4::nc_open(file)
  rad_prof <- ncdf4::ncvar_get(nc, "RAD_PROFILE_CO")
  trad_prof <- t(rad_prof)
  colnames(trad_prof) <- c(
    "PAR_beam_down", "PAR_beam_up",
    "PAR_diff_down", "PAR_diff_up",
    "NIR_beam_down", "NIR_beam_up",
    "NIR_diff_down", "NIR_diff_up",
    "TIR_diff_down", "TIR_diff_up"
  )
  file_workflow <- stringr::str_extract(
    dirname(file),
    "9900[[:digit:]]+"
  ) %>% as.numeric()
  stopifnot(length(file_workflow) == 1, !is.na(file_workflow))
  file_datetime <- stringr::str_extract(
    basename(file),
    "([[:digit:]]+-)+[[:digit:]]+"
  ) %>% lubridate::ymd_hms()
  stopifnot(length(file_datetime) == 1, inherits(file_datetime, "POSIXct"))
  tibble::as_tibble(trad_prof) %>%
    dplyr::mutate(
      workflow_id = file_workflow,
      datetime = file_datetime,
      cohort = dplyr::row_number()
    ) %>%
    dplyr::select(workflow_id, datetime, cohort,
                  dplyr::everything())
}
