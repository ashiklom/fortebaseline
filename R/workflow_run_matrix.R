#' Create a `data.frame` of workflow and run IDs and directories.
#' Useful as an object over which to iterate.
#'
#' @param type Type of output for paths. Either `out` for output or
#'   `run` for run configuration.
#' @return `data.frame` of workflow and run IDs and directory paths
#' @author Alexey Shiklomanov
#' @inheritDotParams workflow_matrix
#' @importFrom magrittr %>%
#' @export
workflow_run_matrix <- function(type = c("out", "run"), ...) {
  type <- match.arg(type)

  workflow_matrix(...) %>%
    dplyr::transmute(
      workflow_id = workflow_id,
      run_paths = purrr::map(path, fs::path, type) %>%
        purrr::map(fs::dir_ls) %>%
        purrr::map(as.character)
    ) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      run_id = fs::path_file(run_paths) %>%
        stringr::str_extract("[[:digit:]]+") %>%
        as.numeric()
    ) %>%
    dplyr::select(workflow_id, run_id,
                  path = run_paths) %>%
    dplyr::filter(!is.na(run_id))
}

#' Create a `data.frame` of workflows
#' 
#' @param path Path to local workflows. Default is
#'   `analysis/data/model_output/workflows`.
#' @param since Date of earliest workflow to use. Default =
#'   `1970-01-01`.
#' @param upto Date of latest workflow to use. Default = `2050-01-01`.
#' @return `data.frame` containing workflow IDs and paths
#' @export
workflow_matrix <- function(path = fs::path("analysis", "data",
                                            "model_output", "workflows"),
                            since = "1970-01-01",
                            upto = "2050-01-01") {
  if (!inherits(since, "POSIX")) {
    since <- tryCatch(
      as.POSIXct(since),
      error = function(e) stop(glue::glue(
        "Unable to parse `{since}` as date. ",
        "Failed with the following error:\n",
        conditionMessage(e)
      ))
    )
  }
  stopifnot(fs::dir_exists(path))

  dir_info <- fs::dir_info(path) %>%
    dplyr::filter(modification_time >= since)
  stopifnot(nrow(dir_info) > 0)

  dir_info %>%
    dplyr::transmute(
      workflow_id = as.numeric(stringr::str_extract(path, "[[:digit:]]+")),
      path = path
    )
}
