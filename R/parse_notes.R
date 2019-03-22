#' Convert a regular `notes` entry to a `data.frame`
#'
#' @param notes Notes entry such as that created by
#'   [[run_ed_ensemble()]], as a single character
#' @return `data.frame` with columns for each setting
#' @author Alexey Shiklomanov
#' @export
parse_notes <- function(notes) {
  dat <- readr::read_delim(
    notes,
    skip = 1,
    delim = ":",
    trim_ws = TRUE,
    col_names = c("setting", "value"),
    col_types = "cl"
  )
  tidyr::spread(dat, setting, value)
}
