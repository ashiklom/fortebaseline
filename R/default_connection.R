#' Default PEcAn database connection
#'
#' @return DBI connection object
#' @author Alexey Shiklomanov
#' @export
default_connection <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    user = "bety",
    password = "bety",
    host = "localhost",
    port = 7990
  )
}
