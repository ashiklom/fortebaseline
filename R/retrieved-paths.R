#' Shortcuts for processed output files
#'
#' @param root Project root directory. Default = `here::here()`.
#' @return `fst::fst` object pointing to dataset
#' @author Alexey Shiklomanov
#' @export
fst_cohort <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-cohort.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_pft <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-pft.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_scalar <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-scalar.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_soil <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-soil.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_cohort_monthly <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-monthly-cohort.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_pft_monthly <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-monthly-pft.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_scalar_monthly <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-monthly-scalar.fst")
  fst::fst(ffile)
}

#' @rdname fst_cohort
#' @export
fst_soil_monthly <- function(root = here::here()) {
  ffile <- retrieved_file("all-output-monthly-soil.fst")
  fst::fst(ffile)
}

retrieved_file <- function(file, root = here::here()) {
  ffile <- file.path(root, "analysis", "data", "retrieved", file)
  stopifnot(file.exists(ffile))
  ffile
}
