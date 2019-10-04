get_timestamp <- function(osf_id) {
  stopifnot(requireNamespace("osfr", quietly = TRUE))
  osfr::osf_retrieve_file(osf_id) %>%
    dplyr::pull(meta) %>%
    purrr::pluck(1, "attributes", "date_modified")
}

osf_url <- function(osf_id) file.path("https://osf.io/download/", osf_id)
