#' PFT information
#'
#' @param col Column to return. If `NULL` (default), return `data.frame`
#' @return If `is.null(col)`, `data.frame` containing PEcAn PFT name
#'   (`bety_name`), formatted name (`pft`), short name (`shortname`),
#'   ED PFT number (`num`), and color (`color`).
#' @author Alexey Shiklomanov
#' @return
#' @export
pfts <- function(col = NULL) {
  long <- c("Early hardwood", "Mid hardwood",
            "Late hardwood", "Pine")
  short <- c("Early", "Mid", "Late", "Pine")
  pfts <- data.frame(
    bety_name = paste0("umbs.", c(
      "early_hardwood",
      "mid_hardwood",
      "late_hardwood",
      "northern_pine"
    )),
    pft = factor(long, long),
    shortname = factor(short, short),
    num = c(9, 10, 11, 6),
    color = viridis::viridis(4),
    stringsAsFactors = FALSE
  )
  if (is.null(col)) return(pfts)
  pfts[[col]]
}

#' Set PFT from ED integer number
#'
#' @param i Vector of PFT integer numbers
#' @return Factor of PFT names
#' @author Alexey Shiklomanov
#' @export
set_pft <- function(i) {
  stopifnot(all(i %in% c(6, 9, 10, 11)))
  factor(i, pfts("num"), pfts("pft"))
}
