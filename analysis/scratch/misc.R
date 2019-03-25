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

##################################################

remove_txt_tag <- function(string, tag) {
  start_rxp <- paste0("<", tag, ">")
  end_rxp <- paste0("</", tag, ">")
  istart <- suppressWarnings(grep(start_rxp, string))
  iend <- suppressWarnings(grep(end_rxp, string))
  stopifnot(length(istart) == length(iend))
  inds <- Reduce(c, Map(seq, istart, iend))
  string[-inds]
}

remove_invalid_xml <- function(string) {
  # Specific unicode characters
  ## rxp <- paste0(
  ##   "[^",
  ##   "\u0001-\uD7FF",
  ##   "\uE000-\uFFFD",
  ##   "\ud800\udc00-\udbff\udfff",
  ##   "]+"
  ## )
  rxp <- "[^-[:alnum:]/<>_.]"
  string <- gsub(rxp, "", string)
  string
}


## proc_text <- remove_txt_tag(raw_txt, "output_filepath") %>%
##   remove_txt_tag("input_filepath") %>%
##   remove_txt_tag("history_out_filepath")
## Encoding(raw_txt) <- "latin1"
## raw_txt_c <- paste(raw_txt, collapse = "\n")
## proc_text <- stringi::stri_trans_general(raw_txt_c, "latin-ascii")
## raw_xml <- xml2::read_xml(proc_txt)
## raw_xml <- xml2::read_xml(paste(proc_txt, collapse = "\n"))
## raw_xml <- XML::xmlParse(proc_txt, asText = TRUE)
## raw_xml <- XML::xmlParse(proc_txt, asText = TRUE)

## raw_xml[[1]]

########################################
workflow_id <- workflow_ids[[3]]
year <- 1902

lai_raw <- ncdf4::ncvar_get(nc, "LAI_PY")
dim(lai_raw)
zz2 <- apply(lai_raw, c(1, 3), sum)
dim(zz)
dim(zz2)
ztboth <- rbind(t(zz[c(6, 9:11), ]), t(zz2[c(6, 9:11), ]))
matplot(ztboth, type = "l")
abline(v = 2 * 24 * 365)
abline(v = 24 * 365, lty = "dashed")
plot(zz, type = 'l')
plot(lai_raw, )

########################################
e2in <- parse_ed2ins(workflow_id)
logfile <- runfile(workflow_id, "logfile.txt")
readLines(logfile) %>% writeLines()
e2in$IMONTHA

########################################
hf <- ncdf4::nc_open("~/Downloads/analysis-T-1902-00-00-000000-g01.h5")
hf$dim$phony_dim_0$len

ncpecan <- ncdf4::nc_open(pecanapi::run_dap(workflow_id, "1903.nc"))
lai <- ncdf4::ncvar_get(ncpecan, "LAI")
plot(lai, type = "l")

##################################################
# Old code for working with ED2IN
parse_ed2ins <- function(workflow_id, con = default_connection()) {
  filepath <- runfile(workflow_id, "ED2IN", con)
  PEcAn.ED2::read_ed2in(filepath)
}

ed2in_dat <- tibble(workflow_id = workflow_ids) %>%
  mutate(ed2in = map(
    workflow_id, possibly(parse_ed2ins, NULL),
    con = default_connection()
  )) %>%
  filter(map_lgl(ed2in, negate(is.null)))

ed2in_full <- ed2in_dat %>%
  mutate(ed2in = map(
    ed2in,
    ~as_tibble(modify_if(.x, ~length(.x) > 1, list))
  )) %>%
  unnest()

run_info <- ed2in_dat %>%
  mutate(
    rtm = map_dbl(ed2in, "ICANRAD"),
    crown_model = map_dbl(ed2in, "CROWN_MOD"),
    n_plant_lim = map_dbl(ed2in, "N_PLANT_LIM"),
    n_decomp_lim = map_dbl(ed2in, "N_DECOMP_LIM"),
    trait_plasticity = map_dbl(ed2in, "TRAIT_PLASTICITY_SCHEME")
    ) %>%
  select(-ed2in)

##################################################
# Old code for reading ED config.xml
all_configs <- tibble(workflow_id = workflow_ids) %>%
  mutate(configs = map(
    workflow_id, possibly(parse_configs, NULL),
    con = default_connection()
  )) %>%
  filter(map_lgl(configs, negate(is.null))) %>%
  unnest()

configs_long <- all_configs %>%
  select(-num) %>%
  select_if(negate(is.list)) %>%
  gather(variable, value, -workflow_id, -pft)

configs_long %>%
  semi_join(
    configs_long %>%
      group_by(pft, variable) %>%
      summarize(variance = var(value)) %>%
      filter(variance != 0)
  ) %>%
  ggplot() +
  aes(x = pft, y = value) +
  geom_jitter() +
  facet_wrap(vars(variable), scales = "free_y")

##################################################
# Old code for reading an ensemble of LAI values
get_ensemble_lai <- function(rundir, years = seq(1902, 1990)) {
  ensemble <- basename(rundir)
  future_map_dfr(years, get_monthly_lai, rundir = rundir) %>%
    dplyr::mutate(ensemble = !!ensemble)
}

##################################################
url_e <- "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000080/out/99000000048/analysis-E-1902-06-00-000000-g01.h5"
hf <- ncdf4::nc_open(url)

names(hf$var)

light <- ncdf4::ncvar_get(hf, "CB_LIGHTMAX")
light

get_rad_profile <- function(workflow_id, year, month) {
  filename <- sprintf("history-S-%d-%02d-01-000000-g01.h5", year, month)
  filepath <- pecanapi::run_dap(workflow_id, filename, port = 7999)
  nc <- ncdf4::nc_open(filepath)
  radprof <- ncdf4::ncvar_get(nc, "RAD_PROFILE_CO")
  radprof
}

all_rad <- purrr::map(7:12, get_rad_profile,
                      workflow_id = 99000000080,
                      year = 1902)


all_rad[[1]]

workflow_id <- 99000000080
year <- 1902
month <- 7

url_s <- "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000080/out/99000000048/history-S-1902-07-01-000000-g01.h5"
histfile <- ncdf4::nc_open(url_s)
h_light <- ncdf4::ncvar_get(histfile, "RAD_PROFILE_CO")
rowMeans(h_light)

url_t <- "http://localhost:7999/thredds/dodsC/outputs/PEcAn_99000000080/out/99000000048/analysis-T-1903-00-00-000000-g01.h5"
hf_t <- ncdf4::nc_open(url_t)
light <- ncdf4::ncvar_get(hf_t, "FMEAN_RAD_PRO")
