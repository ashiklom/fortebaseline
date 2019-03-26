#' Read cohort variables from history file
#'
#' @template workflow_id
#' @param year Output year
#' @param month Output month
#' @param day Output day of month. Default = 1.
#' @param hour Output hour of day. Default = 0 (midnight).
#' @param ... Additional
#' @param varlist List of variables to retrieve.
#' @return `data.frame` of cohort output
#' @author Alexey Shiklomanov
#' @export
read_cohort_history <- function(workflow_id, year, month, day = 1, hour = 0,
                                vars = NULL,
                                .default_vars = c("PFT", "HITE", "LAI_CO",
                                                  "VM0", "SLA")) {
  vars <- toupper(vars)
  filename <- sprintf("history-S-%04d-%02d-%02d-%02d0000-g01.h5", year, month, day, hour)
  url <- pecanapi::run_dap(workflow_id, filename, port = 7999)
  nc <- ncdf4::nc_open(url)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  varlist <- c(toupper(.default_vars), vars)
  out_list <- purrr::map(
    varlist,
    purrr::possibly(ncdf4::ncvar_get, NULL),
    nc = nc
  )
  names(out_list) <- tolower(varlist)
  tibble::tibble(
    workflow_id = workflow_id,
    datetime = ISOdatetime(year, month, day, hour, 0, 0, tz = "UTC"),
    !!!out_list
  )
}
