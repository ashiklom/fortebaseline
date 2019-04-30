# Utilities related to BETY (PEcAn database) connections

#' Shortcut to open a BETY connection
#'
#' @param user,password,host,port Configuration options for BETY
#'   connection. Defaults are configured for A. Shiklomanov's PNNL
#'   instance.
#' @return DBI database connection object.
#' @export
bety <- function(user = "bety", password = "bety", host = "localhost", port = 7990) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    user = user,
    password = password,
    host = host,
    port = port
  )
}

#' Shortcut access functions for BETY table joins
#'
#' @details
#' - `pfts_priors` -- Prior distributions by PFT
#' - `pfts_species` -- PFT-species mapping
#' @param pft_names Character vector of PFT names. Defaults to
#'   `bety_name` column of [pfts()].
#' @param con Database connection. Defaults to [bety()].
#' @param collect Whether or not to call [dplyr::collect()] on the
#'   output. Default = `TRUE`.
#' @name bety_tables
#' @export
pfts_priors <- function(pft_names = pfts("bety_name"), con = bety(),
                        collect = TRUE) {
  pfts <- dplyr::tbl(con, "pfts") %>%
    dplyr::rename(pft_id = id, pft = name) %>%
    dplyr::filter(pft %in% pft_names)
  if (dplyr::pull(dplyr::count(pfts)) == 0) {
    stop("No PFTs with given names found.")
  }
  variables <- dplyr::tbl(con, "variables") %>%
    dplyr::rename(variable_id = id, variable = name)
  priors <- dplyr::tbl(con, "priors") %>%
    dplyr::rename(prior_id = id)
  pfts_priors <- tbl(con, "pfts_priors") %>%
    dplyr::inner_join(
      pfts,
      by = "pft_id",
      suffix = c(".pfts_priors", ".pfts")
    ) %>%
    dplyr::inner_join(
      priors,
      by = "prior_id",
      suffix = c(".pfts_priors", ".priors")
    ) %>%
    dplyr::inner_join(
      variables,
      by = "variable_id",
      suffix = c(".pfts_priors", ".variables")
    )
  out <- pfts_priors %>%
    dplyr::select(pft, variable, distn, parama, paramb, paramc,
                  dplyr::everything())
  if (collect) out <- dplyr::collect(out)
  out
}

#' @rdname bety_tables
#' @export
pfts_species <- function(pft_names = pfts("bety_name"), con = bety(),
                         collect = TRUE) {
  pfts <- dplyr::tbl(con, "pfts") %>%
    dplyr::rename(pft_id = id, pft = name) %>%
    dplyr::filter(pft %in% pft_names)
  species <- tbl(con, "species") %>%
    dplyr::rename(specie_id = id)
  out <- dplyr::tbl(con, "pfts_species") %>%
    dplyr::inner_join(pfts, by = "pft_id", suffix = c(".pfts_species", ".pfts")) %>%
    dplyr::inner_join(species, by = "specie_id", suffix = c(".pfts_species", ".species")) %>%
    dplyr::select(pft, scientificname, dplyr::everything())
  if (collect) out <- dplyr::collect(out)
  out
}

#' Extract BETY PFT, species, or variable IDs given a vector of names
#'
#' @inheritParams bety_tables
#' @param species_names Character vector of species names
#' @param variables Character vector of variable names
#' @return Numeric vector of BETY IDs for given PFTs or species
#' @export
get_pft_ids <- function(pft_names, con = bety()) {
  get_ids(pft_names, "pfts", "name", con = con)
}

#' @rdname get_pft_ids
#' @export
get_species_ids <- function(species_names, con = bety()) {
  get_ids(species_names, "species", "scientificname", con = con)
}

#' @rdname get_pft_ids
#' @export
get_variable_ids <- function(variable_names, con = bety()) {
  get_ids(variable_names, "variables", "name", con = con)
}

#' Retrieve ID from a BETY table given a name
#'
#' @param values Vector of values for which to retrieve IDs
#' @param table Name of table where IDs are stored
#' @param value_col Name of the column corresponding to the values
#' @param id_col Name of the column containing the ID. Default =
#'   `"id"`
#' @inheritParams bety_tables
#' @return Named numeric vector of IDs
#' @export
get_ids <- function(values, table, value_col, id_col = "id", con = bety()) {
  value_col <- dplyr::sym(value_col)
  id_col <- dplyr::sym(id_col)
  dict <- dplyr::tbl(con, table) %>%
    dplyr::filter(!!value_col %in% !!values) %>%
    dplyr::select(!!value_col, !!id_col) %>%
    dplyr::collect() %>%
    tibble::deframe()
  stopifnot(
    length(dict) > 0,
    setequal(names(dict), values)
  )
  dict[values]
}
