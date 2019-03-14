#' Continuously stream the `workflow.Rout` file of a workflow until it
#' is finished
#'
#' @param workflow_id Workflow ID of workflow to monitor
#' @param ... Additional arguments to `pecanapi::output_url`
#' @inheritParams pecanapi::output_url
#' @return Workflow output as character vector, invisibly
#' @author Alexey Shiklomanov
#' @export
follow_workflow <- function(workflow_id,
                            refresh = 2,
                            error_rxp = "> proc.time\\(\\)",
                            start_at = 1,
                            ...) {
  filename <- pecanapi::output_url(workflow_id, "workflow.Rout", ...)
  tail_file(
    filename,
    refresh = refresh,
    error_rxp = error_rxp,
    start_at = start_at
  )
}


#' Follow the output of a file, similar to Linux's `tail -f`
#'
#' @param filename File to follow
#' @param refresh Refresh interval in seconds. Default = 2.
#' @param error_rxp Regular expression to use for completion
#' @param start_at Line at which to start reading the file. Default = 1.
#' @export
tail_file <- function(filename, refresh = 2, error_rxp = NULL, start_at = 1) {
  Sys.sleep(1)
  full_file <- readLines(filename)
  nread <- length(full_file)
  file <- full_file[seq.int(min(start_at, nread), nread)]
  writeLines(file)

  is_done <- FALSE
  need_eol <- FALSE
  while (!is_done) {
    Sys.sleep(refresh)
    update <- readLines(filename)
    nupdate <- length(update)
    if (nupdate > nread) {
      if (need_eol) cat("\r")
      need_eol <- FALSE
      writeLines(update[seq.int(nread + 1, nupdate)])
      nread <- nupdate
    } else {
      need_eol <- TRUE
      cat(".")
    }
    if (!is.null(error_rxp) && any(grepl(error_rxp, update))) is_done <- TRUE
  }
  invisible(update)
}
