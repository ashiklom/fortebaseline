#' Set PFT from ED integer number
#'
#' @param i Vector of PFT integer numbers
#' @return Factor of PFT names
#' @author Alexey Shiklomanov
#' @export
set_pft <- function(i) {
  stopifnot(all(i %in% c(6, 9, 10, 11)))
  pfts <- c(
    rep(NA_character_, 5),
    "Pine", NA_character_, NA_character_,
    "Early hardwood", "Mid hardwood", "Late hardwood"
  )
  ipfts <- pfts[i]
  factor(ipfts, pfts[c(9:11, 6)])
}
