library(drake)
library(ggplot2)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

import::from(magrittr, "%>%")
options("pecanapi.docker_port" = 7999)

ystart <- function(year, tz = "UTC") ISOdatetime(year, 1, 1, 0, 0, 0, tz = tz)

read_soil <- function(year) {
  filename <- sprintf(
    "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000032/out/99000000030/analysis-T-%d-00-00-000000-g01.h5",
    year
  )
  hf <- ncdf4::nc_open(filename)
  on.exit(ncdf4::nc_close(hf))
  soil_raw <- ncdf4::ncvar_get(hf, "FMEAN_SOIL_WATER_PY")
  t(soil_raw) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      time = ystart(year) + 30 * lubridate::minutes(dplyr::row_number())
    ) %>%
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
  raw_soil = purrr::map(run_years, purrr::safely(read_soil)),
  soil_output = raw_soil %>%
    purrr::map_if(., ~is.null(.[["error"]]), "result") %>%
    dplyr::bind_rows(),
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
make(plan)
readd(soil_output)

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
