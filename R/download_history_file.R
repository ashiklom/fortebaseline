#' Download a history output file for a given workflow
#'
#' @template workflow_id
#' @param year Output year
#' @param month Output month
#' @param day Output day
#' @param hour Output hour
#' @param outdir Target directory for storing files. Default is
#'   `analysis/data/model_output/histfiles`
#' @return Full path to downloaded file
#' @author Alexey Shiklomanov
#' @export
download_history_file <- function(workflow_id, year, month, day, hour,
                                  outdir = file.path("analysis", "data",
                                                     "model_output", "histfiles")) {
  filename <- sprintf("history-S-%04d-%02d-%02d-%02d0000-g01.h5",
                      year, month, day, hour)
  prun_url <- purrr::insistently(pecanapi::run_url, purrr::rate_backoff(max_times = 10))
  url <- prun_url(workflow_id, filename, port = 7999)
  full_outdir <- file.path(outdir, workflow_id)
  dir.create(full_outdir, showWarnings = FALSE, recursive = TRUE)
  outfile <- file.path(full_outdir, filename)
  download.file(url, outfile)
  outfile
}
