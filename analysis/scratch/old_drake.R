library(fortebaseline)
library(drake)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

stopifnot(
  requireNamespace("glue"),
  requireNamespace("progress")
)

import::from(magrittr, "%>%")

read_ed_variables <- function(year, workflow_id, run_id,
                              variables = NULL,
                              ftype = "T",
                              listify = TRUE,
                              pb = NULL) {
  if (!is.null(pb)) pb$tick()
  filename <- glue::glue(
    "http://localhost:{getOption('pecanapi.docker_port')}",
    "/thredds/dodsC/outputs/",
    "PEcAn_{workflow_id}/out/{run_id}/analysis-{ftype}-{year}-00-00-000000-g01.h5"
  )
  hf <- ncdf4::nc_open(filename)
  on.exit(ncdf4::nc_close(hf), add = TRUE)
  if (is.null(variables)) variables <- names(hf[["var"]])
  outlist <- list(year = year)
  for (v in variables) {
    value <- tryCatch({
      ncdf4::ncvar_get(hf, v)
    },
    error = function(e) {
      message("Variable ", v, "hit the following error:\n", e)
      message("Returning NULL")
      return(NULL)
    })
    if (listify && length(value) > 1) value <- list(value)
    outlist[[v]] <- value
  }
  outlist
}

add_ed_timestep <- function(data, year) {
  stopifnot(is.data.frame(data), is.numeric(year), length(year) == 1)
  dplyr::mutate(
    data,
    time = qdate(year) + 30 * lubridate::minutes(dplyr::row_number())
  )
}

read_soil <- function(year, workflow_id, run_id) {
  soil_raw <- read_ed_variables(
    year,
    variables = "FMEAN_SOIL_WATER_PY",
    workflow_id = workflow_id,
    run_id = run_id
  )[[1]]
  t(soil_raw) %>%
    tibble::as_tibble() %>%
    add_ed_timestep(year) %>%
    tidyr::gather(layer, value, -time) %>%
    dplyr::mutate(layer = as.factor(layer))
}

plan <- drake_plan(
  workflow_id = 99000000032,
  run_id = 99000000030,
  run_years = seq(1902, 2000),
  file_names = pecanapi::run_dap(
    workflow_id,
    paste0(run_years, ".nc"),
    run_id = run_id
  ),
  raw_output = target(
    command = PEcAn.utils::read.output(
      ncfiles = file_names,
      variables = NULL,                   # All variables
      verbose = TRUE,
      dataframe = TRUE
    ), trigger = trigger(condition = FALSE, mode = "condition")),
  raw_year_output = purrr::map(
    run_years,
    read_ed_variables,
    workflow_id = workflow_id,
    run_id = run_id,
    ftype = "Y"
    ## pb = pb_along(run_years),
  ),
  raw_soil = purrr::map(
    run_years,
    purrr::safely(read_soil),
    workflow_id = workflow_id,
    run_id = run_id
    ## pb = pb_along(run_years)
  ),
  soil_output = raw_soil %>%
    purrr::map_if(., ~is.null(.[["error"]]), "result") %>%
    dplyr::bind_rows(),
  soil_monthly = soil_output %>%
    dplyr::mutate(
      year = lubridate::year(time),
      month = lubridate::month(time),
      ymonth = ISOdate(year, month, 01, tz = "UTC")
    ) %>%
    dplyr::group_by(ymonth, layer) %>%
    dplyr::summarize(
      mean = mean(value),
      median = median(value),
      hi = quantile(value, 0.95),
      lo = quantile(value, 0.05)
    ),
  daily_output = raw_output %>%
    dplyr::mutate(date = lubridate::as_date(posix)) %>%
    dplyr::select(-posix, -year) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize_all(mean, na.rm = TRUE),
  monthly_output = raw_output %>%
    dplyr::mutate(month = lubridate::month(posix)) %>%
    dplyr::select(-posix) %>%
    dplyr::group_by(month, year) %>%
    dplyr::mutate(my = ISOdate(year, month, 1)) %>%
    dplyr::summarize_all(mean, na.rm = TRUE),
  annual_output = raw_output %>%
    dplyr::select(-posix) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize_all(mean, na.rm = TRUE),
  )
plan_config <- drake_config(plan)
make(plan, targets = "raw_year_output")

readd(soil_monthly) %>%
  ggplot() +
  aes(x = ymonth, y = mean, ymin = lo, ymax = hi) +
  geom_line() +
  facet_wrap(~layer)

readd(annual_output) %>%
  dplyr::select(year, GPP, NPP, TotalResp, TotSoilCarb, LAI, Tair, Rainf) %>%
  tidyr::gather(variable, value, -year) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y")

readd(annual_output) %>%
  dplyr::select(-year) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(type = "lower")

readd(annual_output) %>%
  dplyr::select(year, LAI, Wa) %>%
  tidyr::gather(variable, value, -year) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y")

readd(monthly_output) %>%
  dplyr::ungroup() %>%
  dplyr::select(my, LAI, WaterTableD) %>%
  tidyr::gather(variable, value, -my) %>%
  ggplot() +
  aes(x = my, y = value) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y")

ggsave("analysis/figures/ed_cruncep_climate.pdf", width = 8, height = 5)

readd(daily_output) %>%
  ggplot() +
  aes(x = date, y = Wind) +
  geom_line()

readd(daily_output) %>% dplyr::glimpse()

## raw_output = PEcAn.utils::read.output(
##   ncfiles = readd(file_names),
##   variables = NULL,                   # All variables
##   verbose = TRUE,
##   dataframe = TRUE
## )
