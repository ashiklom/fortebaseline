#' Run PEcAn trait meta analysis for a specific PFT
#'
#' @param con DBI connection object to PEcAn database
#' @param pft Plant functional type name
#' @return Raw meta analysis output (see [PEcAn.MA::pecan.ma()])
#' @author Alexey Shiklomanov
#' @export
pecan_ma_pft <- function(con, pft) {
  stopifnot(
    requireNamespace("PEcAn.DB", quietly = TRUE),
    requireNamespace("PEcAn.MA", quietly = TRUE)
  )
  pft_id <- PEcAn.DB::db.query("SELECT id FROM pfts WHERE name = $1",
                               con, values = list(pft))[[1]]
  stopifnot(length(pft_id) == 1)
  species <- PEcAn.DB::query.pft_species("umbs.early_hardwood", con = con)
  priors <- PEcAn.DB::query.priors(pft_id, con = con)
  trait_data <- PEcAn.DB::query.traits(
    species[["id"]],
    rownames(priors),
    con = con
  )
  jagged_data <- lapply(trait_data, PEcAn.MA::jagify)
  taupriors <- list(
    tauA = 0.01,
    tauB = setNames(rep(0.01, nrow(priors)), rownames(priors))
  )
  outdir <- tempdir()
  PEcAn.MA::pecan.ma(
    jagged_data, priors,
    taupriors = taupriors,
    j.iter = 3000,
    outdir = outdir,
    logfile = NULL
  )
}

#' Summarize meta analysis output for multiple PFTs as a tidy `data.frame`
#'
#' @param result List of PFT-level meta-analysis results
#' @return `data.frame` of summary statistics from meta analysis
#' @export
summarize_ma <- function(result) {
  result %>%
    purrr::map_dfr(summarize_pft_ma, .id = "trait") %>%
    dplyr::select(trait, variable, Mean) %>%
    tidyr::spread(variable, Mean)
}

summarize_pft_ma <- function(pft_result) {
  smry <- summary(pft_result)
  dfs <- purrr::map(smry[1:2], tibble::as_tibble, rownames = "variable")
  dplyr::left_join(dfs[[1]], dfs[[2]], by = "variable")
}
