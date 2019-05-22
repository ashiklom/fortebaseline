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
  species <- PEcAn.DB::query.pft_species(pft, con = con)
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
  ma_result <- PEcAn.MA::pecan.ma(
    jagged_data, priors,
    taupriors = taupriors,
    j.iter = 3000,
    outdir = outdir,
    logfile = NULL
  )

  posterior <- PEcAn.MA::approx.posterior(ma_result, priors) %>%
    tibble::as_tibble(rownames = "trait")

  list(ma_result = ma_result, posterior = posterior, priors = priors)
}

#' Convert list of meta-analysis results to a tidy `data.frame` of the
#' posterior distribution.
#'
#' @param ma_results List of meta-analysis results for multiple PFTs
#' @param ndraws Number of random draws for calculating summary stats
#'   (default = 5000).
#' @return Tidy `data.frame` containing posterior information, draws,
#'   and summary statistics
#' @export
tidy_posterior <- function(ma_results, ndraws = 5000) {
  posteriors <- ma_results %>%
    purrr::map_dfr("posterior", .id = "pft")
  draw_traits(posteriors, ndraws = ndraws)
}

#' Draw traits from data frame describing prior or posterior
#'
#' @param data `data.frame` describing distributions, with columns
#'   `distn` (name of distribution function), `parama` and `paramb`
#'   (distribution parameters)
#' @inherit tidy_posterior params return
#' @export
draw_traits <- function(data, ndraws = 5000) {
  assertthat::assert_that(
    assertthat::has_name(data, "distn"),
    assertthat::has_name(data, "parama"),
    assertthat::has_name(data, "paramb")
  )
  data %>%
    dplyr::mutate(
      rfun = paste0("r", distn),
      draws = purrr::pmap(list(rfun, parama, paramb),
                          ~do.call(..1, list(ndraws, ..2, ..3))),
      Mean = purrr::map_dbl(draws, mean),
      Median = purrr::map_dbl(draws, median),
      SD = purrr::map_dbl(draws, sd),
      lo = purrr::map_dbl(draws, quantile, 0.025),
      hi = purrr::map_dbl(draws, quantile, 0.975)
    )
}
