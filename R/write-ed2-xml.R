#' Write ED2 XML file
#'
#' @param trait_values
#' @return
#' @author Alexey Shiklomanov
#' @export
write_ed2_xml <- function(trait_values) {
  pft_list <- tibble::tibble(bety_name = names(trait_values)) %>%
    dplyr::left_join(pfts(), by = "bety_name") %>%
    dplyr::select(name = bety_name, ed2_pft_number = num) %>%
    purrr::transpose()
  settings <- list(
    model = list(revision = "git"),
    pfts = pft_list
  )
  write.config.xml.ED2(settings, trait_values)
}

#' Convert `data.frame` to PFT list suitable for `write_ed2_xml` input
#'
#' @param dat `data.frame` with PFT trait data
#' @return Named nested list of PFTs and trait values
#' @author Alexey Shiklomanov
#' @export
as_pft_list <- function(dat) {
  names <- dat[["name"]]
  stopifnot(!is.null(names))
  vals <- dat %>%
    dplyr::select(-name) %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::map(purrr::discard, is.na)
  names(vals) <- names
  vals
}
