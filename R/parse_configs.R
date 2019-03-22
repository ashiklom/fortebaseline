#' Parse ED `config.xml` files
#'
#' @param workflow_id Target workflow ID
#' @param con Database connection. Default is [default_connection()].
#' @param pfts PFTs to use. If `NULL`, use default PFTs: Early, mid,
#'   and late hardwoods, and pine, in that order.
#' @return `data.frame` of trait values for that run
#' @author Alexey Shiklomanov
#' @export
parse_configs <- function(workflow_id, con = default_connection(),
                          pfts = NULL) {
  if (is.null(pfts)) {
    pfts <- c("Early hardwood", "Mid hardwood", "Late hardwood", "Pine")
  }
  filepath <- runfile(workflow_id, "config.xml", con)
  conf_raw <- xml2::read_xml(filepath) %>%
    xml2::as_list()
  conf_raw[[1]] %>%
    setNames(pfts) %>%
    purrr::map_dfr(~tibble::tibble(!!!.x), .id = "pft") %>%
    dplyr::mutate_at(vars(-pft), purrr::simplify) %>%
    dplyr::mutate_at(vars(-pft), try_numeric)
}
