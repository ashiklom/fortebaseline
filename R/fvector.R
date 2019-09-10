#' Create a variable-index vector or matrix
#'
#' @param indices Vector indices
#' @param irows,icols Indices of rows or columns
#' @return Numeric vector with "custom" indices
#' @author Alexey Shiklomanov
#' @examples
#' x <- fvector(0:2)
#' x[] <- c(5, 7, 9)
#' x
#' x[0]
#' # This matrix is square
#' y <- fmatrix(0:5, 1:6)
#' @export
fvector <- function(indices) {
  stopifnot(
    is.integer(indices),
    all(diff(indices) == 1)
  )
  structure(
    numeric(length(indices)),
    offset = 1 - min(indices),
    class = c("fvector", "numeric")
  )
}

#' @rdname fvector
#' @export
fmatrix <- function(irows, icols) {
  stopifnot(
    is.integer(irows),
    is.integer(icols),
    all(diff(irows) == 1),
    all(diff(icols) == 1)
  )
  structure(
    numeric(length(irows) * length(icols)),
    dim = c(length(irows), length(icols)),
    offset = c(1 - min(irows), 1 - min(icols)),
    class = c("fmatrix", "matrix")
  )
}

print.fvector <- function(x, ...) {
  names(x) <- as.character(seq_along(x) - attr(x, "offset"))
  attr(x, "offset") <- NULL
  class(x) <- setdiff(class(x), "fvector")
  NextMethod(x)
}

"[.fvector" <- function(x, i) {
  offset <- attr(x, "offset")
  if (is.logical(i)) NextMethod()
  if (missing(i)) {
    i <- seq_along(x) - offset
  }
  i <- i + offset
  NextMethod()
}

"[.fmatrix" <- function(x, i, j) {
  offset <- attr(x, "offset")
  if (missing(i)) {
    i <- seq_len(nrow(x)) - offset[1]
  }
  if (missing(j)) {
    j <- seq_len(ncol(x)) - offset[2]
  }
  if (!is.logical(i)) i <- i + offset[1]
  if (!is.logical(j)) j <- j + offset[2]
  NextMethod()
}

"[<-.fvector" <- function(x, i, value) {
  offset <- attr(x, "offset")
  if (missing(i)) {
    i <- seq_along(x) - offset
  }
  i <- i + offset
  NextMethod()
}

"[<-.fmatrix" <- function(x, i, j, value) {
  offset <- attr(x, "offset")
  if (missing(i)) {
    i <- seq_along(nrow(x) - offset[1])
  }
  i <- i + offset[1]
  if (missing(j)) {
    j <- seq_along(ncol(x) - offset[2])
  }
  j <- j + offset[2]
  NextMethod()
}
