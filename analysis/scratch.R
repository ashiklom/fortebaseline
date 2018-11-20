# Read CRUNCEP wind inputs. Is the same wind time series looped
# from 1902 to 1948? 
winds <- list()
files <- glue::glue("http://localhost:7999/thredds/dodsC/dbfiles/CRUNCEP_site_1-33/CRUNCEP.{1903:1910}.nc")
for (yr in 1903:1951) {
  file <- glue::glue("http://localhost:7999/thredds/dodsC/dbfiles/CRUNCEP_site_1-33/CRUNCEP.{yr}.nc")
  nc <- ncdf4::nc_open(file)
  winds[[as.character(yr)]] <- tibble::tibble(
    year = yr,
    north = ncdf4::ncvar_get(nc, "northward_wind"),
    east = ncdf4::ncvar_get(nc, "eastward_wind"),
    rn = seq_along(north)
  )
}
wind_df <- dplyr::bind_rows(winds) %>%
  dplyr::mutate(datetime = ISOdate(year, 1, 1, 0, 0, 0, tz = "UTC") + 4 * lubridate::hours(rn))

wind_df %>%
  dplyr::filter(year > 1940) %>%
  ggplot() +
  aes(x = rn, y = north) +
  geom_line() +
  facet_grid(year ~ .)

ggplot(wind_df) +
  aes(x = rn, y = north, color = year <= 1950) +
  geom_line()

# Yes, yes it is.
############################################################
# Is read.output actually loading everything it can from the ED output?
# No! 

filename <- "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000032/out/99000000030/analysis-T-1932-00-00-000000-g01.h5"
hf <- ncdf4::nc_open(filename)
soil_moist <- ncdf4::ncvar_get(hf, "FMEAN_SOIL_WATER_PY")
ncdf4::nc_close(hf)

tsoil_moist <- tibble::as_tibble(t(soil_moist)) %>%
  dplyr::mutate(t = dplyr::row_number()) %>%
  tidyr::gather(layer, value, -t) %>%
  dplyr::mutate(layer = as.factor(layer))

ggplot(tsoil_moist) +
  aes(x = t, y = value, color = layer) +
  geom_line() +
  scale_color_viridis_d()

tot_soil_moist <- colSums(soil_moist)
plot(tot_soil_moist, type = "l")
