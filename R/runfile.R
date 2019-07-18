#' Retrieve all the `run` files from a particular workflow.
#'
#' @template workflow_id
#' @param target
#' @template con
#' @return Character vector of URLs for reading or downloading files.
#' @author Alexey Shiklomanov
#' @export
runfiles <- function(workflow_id, target, con = bety()) {
  runs <- pecanapi::list_runs(con, workflow_id)
  if (nrow(runs) == 0) return(NULL)
  purrr::map_chr(file.path("run", runs[["id"]], target),
                 pecanapi::output_url,
                 workflow_id = workflow_id)
}
