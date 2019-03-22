#' Read monthly LAI an ED `-T-` file
#'
#' @template workflow_id
#' @param year Target year
#' @param startmonth Start month for reading in data. Default = 1.
#' @param startday Start day for reading in data. Default = 1.
#' @return `data.frame` of monthly LAI values by PFT
#' @author Alexey Shiklomanov
#' @export
get_monthly_lai <- function(workflow_id, year,
                            startmonth = 1,
                            startday = 1) {

  # Number of days from start date to end of current year 
  nday <- as.numeric(difftime(
    sprintf("%d-01-01", year + 1),
    sprintf("%d-%d-%d", year, startmonth, startday),
    tz = "UTC",
    units = "days"
  ))
  nt <- nday * 24 * 2 # Half-hourly output

  filename <- glue::glue("analysis-T-{year}-00-00-000000-g01.h5")
  filepath <- pecanapi::run_dap(workflow_id, filename, port = 7999)

  nc <- ncdf4::nc_open(filepath)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  lai_raw <- ncdf4::ncvar_get(
    nc,
    "LAI_PY",
    start = c(6, 1, 1, 1),
    count = c(6, -1, -1, nt) # Only include PFTs 6, (7), 8, 9, 10, 11
  )[-2,,] # Drop PFT 7
  pft_levels <- c("Early hardwood", "Mid hardwood", "Late hardwood",
                  "Pine", "Late conifer")
  pftnames <- factor(pft_levels[c(4, 5, 1:3)], pft_levels)
  basedate <- ISOdate(year, startmonth, 1, 0, 0, 0, "UTC")

  data.table::melt(lai_raw) %>%
    tibble::as_tibble() %>%
    dplyr::rename(ipft = Var1, cohort = Var2, timestep = Var3) %>%
    dplyr::mutate(
      pft = pftnames[ipft],
      dt = basedate + lubridate::dhours((timestep - 1) / 2),
      month = lubridate::floor_date(dt, "month")
    ) %>%
    dplyr::group_by(month, pft, cohort) %>%
    dplyr::summarize(lai = mean(value)) %>%
    dplyr::summarize(lai = sum(lai)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(workflow_id = !!workflow_id)
}
