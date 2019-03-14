#' Read the workflow notes column of a FoRTE run
#'
#' @param workflow_id Workflow ID of target workflow
#' @inheritParams run_ed_ensemble
#' @return Named list describing the ED configuration of that workflow
#' @author Alexey Shiklomanov
#' @export
read_notes <- function(workflow_id, con = NULL) {
  if (is.null(con)) con <- default_connection()
  workflows <- dplyr::tbl(con, "workflows") %>%
    dplyr::filter(id == workflow_id) %>%
    dplyr::collect()
  if (nrow(workflows) == 0) {
    stop(glue::glue("Workflow ID `{workflow_id}` not found."))
  }
  notes <- workflows[["notes"]]
  if (is.null(notes)) {
    warning(glue::glue("Notes column in workflow `{workflow_id}` is NULL"))
    return(NULL)
  }
  all_tags <- strsplit(notes, "\n")[[1]]
  if (all_tags[[1]] != "==FoRTE run==") {
    warning(glue::glue("Notes column in workflow `{workflow_id}` is missing ",
                       "the FoRTE tag. Returning `NULL`."))
    return(NULL)
  }
  tags <- lapply(strsplit(all_tags[-1], ":"), trimws)
  tag_names <- vapply(tags, `[[`, character(1), 1)
  tag_values <- vapply(tags, function(x) as.logical(x[[2]]), logical(1))
  names(tag_values) <- tag_names
  tag_values
}
