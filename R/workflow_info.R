#' Retrieve table of model structure information for each workflow
#'
#' @param ... Arguments to [workflow_matrix()].
#' @return `data.frame` of workflow ID and information about model structure
#' @author Alexey Shiklomanov
#' @export
workflow_structures <- function(...) {
  workflow_matrix(...) %>%
    dplyr::mutate(
      notes = purrr::map(
        fs::path(path, "pecan.xml"),
        purrr::compose(parse_notes,
                       purrr::as_mapper(list("pecan", "info", "notes", 1)),
                       xml2::as_list,
                       xml2::read_xml)
      )
    ) %>%
    dplyr::select(workflow_id, notes) %>%
    tidyr::unnest(notes) %>%
    transmute(
      workflow_id = workflow_id,
      crown = fct_inorder(if_else(crown_model, "finite", "closed")),
      rtm = fct_inorder(if_else(multiple_scatter, "multi-scatter", "two-stream")),
      traits = fct_inorder(if_else(trait_plasticity, "plastic", "static"))
    )
}
