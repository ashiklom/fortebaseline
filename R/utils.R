#' Create datetime object from isolated pieces
#'
#' Thin wrapper around [base::ISOdatetime()] that provides default
#' arguments. All arguments are vectorized.
#' 
#' @param year Year (numeric)
#' @param month Month (numeric, default = 1)
#' @param day Day of the month (numeric, default = 1)
#' @param hour Hour (numeric, default = 0)
#' @param minute Minute (numeric, default = 0)
#' @param second Second (numeric, default = 0)
#' @param tz Time zone (character, default = "UTC")
#' @inherit base::ISOdatetime return
#' @author Alexey Shiklomanov
#' @export
#' @examples
#' # Create dates given only the years (midnight, January 1)
#' qdate(2005:2010)
#' # Every September 5 (midnight)
#' qdate(2005:2010, 9, 5)
#' # ...at noon
#' qdate(2005:2010, 9, 5, 12)
qdate <- function(year,
                  month = 1,
                  day = 1,
                  hour = 0,
                  minute = 0,
                  second = 0,
                  tz = "UTC") {
  ISOdatetime(year, month, day, hour, minute, second, tz = tz)
}

#' Create progress bar object with length equal to target object
#'
#' @param x Object "along" which to create progress bar
#' @param ... Additional arguments to `progress::progress_bar$new()`
#' @return `progress` object (see [progress::progress_bar()]).
#' @author Alexey Shiklomanov
#' @export
pb_along <- function(x, ...) {
  progress::progress_bar$new(total = length(x), ...)
}
