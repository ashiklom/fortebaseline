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

#########################################

library(pecanapi)
library(magrittr)

options(pecanapi.user_id = 99000000002,
        pecanapi.docker_port = 7999)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

model_id <- get_model_id(con, "ED2", "develop")
site_id <- 1000000033

workflow <- insert_new_workflow(
  con,
  site_id,
  model_id,
  start_date = "2004-07-01",
  end_date = "2004-08-01"
)
workflow_id <- workflow[["id"]]

settings <- list() %>%
  add_workflow(workflow) %>%
  add_database() %>%
  add_pft("Optics.Temperate_Early_Hardwood") %>%
  add_rabbitmq(con = con) %>%
  modifyList(list(
    meta.analysis = list(iter = 3000, random.effects = FALSE),
    run = list(inputs = list(met = list(source = "CRUNCEP", output = "ED2", method = "ncss"),
                             lu = list(id = 294),
                             soil = list(id = 297),
                             thsum = list(id = 295),
                             veg = list(id = 296))),
    ensemble = list(size = 10, variable = "NPP"),
    model = list(revision = "git",
                 prerun = "ulimit -s unlimited")
  ))

submit_workflow(settings)
watch_workflow(workflow_id)

##################################################
# Related to reading ED 
##################################################
ed_summary %>%
  filter(variable == "nee") %>%
  ggplot() +
  aes(x = ymonth, y = value_mean) +
  geom_col()


time <- read_nc_time(nc)
npp <- ncdf4::ncvar_get(nc, "NPP")
lai <- ncdf4::ncvar_get(nc, "LAI")

## nc <- ncdf4::nc_open(run_dap(workflow_id, "1902.nc", run1))

## catalog_url <- pecanapi:::thredds_fs_url()
## raw_catalog <- 

## con <- DBI::dbConnect(
##   RPostgres::Postgres(),
##   user = "bety",
##   password = "bety",
##   host = "localhost",
##   port = 7990
## )

##################################################
workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
runs <- list.files(outdir)

run <- runs[[1]]

rundir <- file.path(outdir, run)
ncfiles <- list.files(rundir, "[[:digit:]]+\\.nc$",
                      full.names = TRUE)

x <- stars::read_stars(ncfiles, quiet = TRUE, proxy = TRUE,
                       along = "time")
