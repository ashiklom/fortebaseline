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
