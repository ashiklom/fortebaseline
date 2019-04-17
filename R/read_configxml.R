#' Read ED2 parameter file (`config.xml`) into a `data.frame`
#'
#' @param path Path to run directory containing XML config file
#' @param filename Name of config file. Default = `config.xml`
#' @return Wide `data.frame` containing parameters for each PFT.
#' @author Alexey Shiklomanov
#' @importFrom magrittr "%>%"
#' @export
read_configxml <- function(path, filename = "config.xml") {
  config_file <- fs::path(path, filename)
  stopifnot(fs::file_exists(config_file))
  xml <- xml2::read_xml(config_file) %>% xml2::as_list()
  x <- out$leaf_respiration_rate_m2
  xml[["config"]] %>%
    purrr::map_dfr(as_tibble) %>%
    dplyr::mutate_all(purrr::simplify) %>%
    ## Missing parameters are set to `NULL`, which `simplify` can't
    ## handle. Convert to NA and then re-simplify only those columns.
    dplyr::mutate_if(is.list, ~purrr::map_if(.x, is.null, ~NA_character_)) %>%
    dplyr::mutate_if(is.list, purrr::simplify) %>%
    dplyr::mutate_all(readr::parse_guess) %>%
    dplyr::mutate_if(is.character, readr::parse_guess) %>%
    dplyr::mutate(pft = set_pft(num)) %>%
    dplyr::select(pft, everything(), -num)
}
