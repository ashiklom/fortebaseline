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
parse_ed2ins <- function(workflow_id, con = bety()) {
  filepath <- runfile(workflow_id, "ED2IN", con)
  PEcAn.ED2::read_ed2in(filepath)
}

ed2in_dat <- tibble(workflow_id = workflow_ids) %>%
  mutate(ed2in = map(
    workflow_id, possibly(parse_ed2ins, NULL),
    con = bety()
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
    con = bety()
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

##################################################
wf_sub <- workflow_df %>%
  filter(!(n_limit_soil & !n_limit_ps)) %>%
  select(-dplyr::ends_with("_date")) %>%
  dplyr::arrange(workflow_id)

ids <- wf_sub %>%
  filter(!trait_plasticity, n_limit_ps, !n_limit_soil)
# 88, 90, 104, 106

wfid <- 99000000088
## outfile <- pecanapi::run_dap(wfid, "analysis-D-1910-07-05-000000-g01.h5")
hist_dates <- as_tibble(expand.grid(time = seq(0, 18, 6), day = seq(1, 10)))
files <- with(hist_dates, sprintf("history-S-1910-07-%02d-%02d0000-g01.h5", day, time))

outdir <- file.path("analysis/data/model_output/histfiles/", wfid)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
for (f in files) {
  message("Processing file: ", f)
  url <- pecanapi::run_url(wfid, f)
  outfile <- file.path(outdir, f)
  if (!file.exists(outfile)) download.file(url, outfile)
  ## nc <- ncdf4::nc_open(url)
  ## l[[f]] <- ncdf4::ncvar_get(nc, "RAD_PROFILE_CO")
  ## ncdf4::nc_close(nc)
}

result <- purrr::map(
  files,
  function(x) {
    path <- pecanapi::run_dap(wfid, x)
    ## path <- file.path(outdir, x)
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    ncdf4::ncvar_get(nc, "RAD_PROFILE_CO")
  }
)
names(result) <- files

tidy_rad_profile <- function(rad_prof) {
  trad_prof <- t(rad_prof)
  colnames(trad_prof) <- c(
    "PAR_beam_down", "PAR_beam_up",
    "PAR_diff_down", "PAR_diff_up",
    "NIR_beam_down", "NIR_beam_up",
    "NIR_diff_down", "NIR_diff_up",
    "TIR_diff_down", "TIR_diff_up"
  )
  tibble::as_tibble(trad_prof) %>%
    dplyr::mutate(cohort = dplyr::row_number()) %>%
    dplyr::select(cohort, dplyr::everything())
}

result_tidy <- purrr::map_dfr(result, tidy_rad_profile, .id = "file") %>%
  dplyr::mutate(datetime = file %>%
                  stringr::str_extract( "([[:digit:]]+-)+[[:digit:]]+") %>%
                  lubridate::ymd_hms()) %>%
  dplyr::select(datetime, cohort, dplyr::everything())

##################################################
# Plastic traits
##################################################
library(tidyverse)

year <- 1910
month <- 7
workflow_id <- readd(plastic_wfids)[[3]]
filename <- sprintf("history-S-%04d-%02d-%02d-%02d0000-g01.h5", year, month, 1, 0)
url <- pecanapi::run_dap(workflow_id, filename, port = 7999)
nc <- ncdf4::nc_open(url)
ncdf4::nc_close(nc)

hist_matrix <- as_tibble(expand.grid(
  workflow_id = pull(workflows_years, workflow_id) %>% head(3),
  year = seq(1902, 1905),
  month = seq(6, 8)
))

future::plan(future.callr::callr)

cohorts <- pmap(hist_matrix, with_prog(safely(read_cohort_history), .pb = pbn(NROW(hist_matrix))))
cohorts <- future_map(hist_matrix, safely(read_cohort_history))
cohort_data <- cohorts %>%
  discard(~is.null(.x[["result"]])) %>%
  map_dfr("result")
## cohorts <- purrr::pmap(hist_matrix, with_prog(read_cohort_history, .pb = pbn(NROW(hist_matrix))))

cohort_data %>%
  group_by(pft) %>%
  summarize(vm0 = var(vm0), sla = var(sla))

cohort_data %>%
  group_by(workflow_id, datetime) %>%
  mutate(cohort = row_number()) %>%
  ungroup(workflow_id, datetime) %>%
  ggplot() +
  aes(x = datetime, y = lai_co, color = factor(pft)) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(workflow_id), scales = "free_y")

readd(workflow_df) %>%
  filter(workflow_id == 99000000089) %>%
  dplyr::glimpse()

##################################################
# Old ED ensemble output
ed_out <- read_fst("analysis/data/derived-data/ed-ensemble-out.fst") %>%
  as_tibble()

old_ensemble_summary <- ed_out %>%
  spread(variable, value) %>%
  group_by(run_id, year = lubridate::floor_date(time, "year")) %>%
  summarize(npp = sum(npp), gpp = sum(gpp), lai = max(lai)) %>%
  ungroup() %>%
  gather(variable, value, npp, gpp, lai)

old_ensemble_summary %>%
  mutate(variable = factor(variable, c("gpp", "npp", "lai")) %>%
           lvls_revalue(c("GPP", "NPP", "LAI"))) %>%
  ggplot() +
  aes(x = year, y = value, group = run_id) +
  geom_line(alpha = 0.2) +
  facet_wrap(vars(variable), scales = "free") +
  theme_cowplot() +
  theme(axis.title.x = element_blank())

old_ensemble_summary %>%
  filter(year == floor_date(year, "10 years")) %>%
  ggplot() +
  aes(x = factor(strftime(year, "%Y")), y = value) +
  geom_violin() +
  ## geom_jitter() +
  facet_wrap(vars(variable), scales = "free")

##################################################
library(drake)

dplan <- drake_plan(
  cdat = target(
    mutate(read_month(file_in(file)),
           workflow = workflow,
           run = run,
           date = date),
    transform = map(
      file = !!inputs$month_path,
      workflow = !!inputs$workflows,
      run = !!inputs$runs,
      date = !!inputs$date
    )
  ),
  cohort_data = target(
    bind_rows(cdat),
    transform = combine(cdat)
  )
)

## dconf <- drake_config(dplan, parallelism = "future", jobs = parallel::detectCores())
## make(config = dconf)
future::plan("multiprocess")
make(dplan, parallelism = "future", jobs = parallel::detectCores())

##################################################

library(tidyverse)
library(fortebaseline)

result_dir <- file.path("analysis", "data", "model_output", "workflows")
wf <- tibble(
  workflows = list.files(result_dir),
  workflow_id = as.numeric(gsub("PEcAn_", "", workflows)),
  workflow_paths = file.path(result_dir, workflows),
  notes = purrr::map(file.path(workflow_paths, "pecan.xml"),
                     purrr::compose(parse_notes,
                                    purrr::as_mapper(list("pecan", "info", "notes", 1)),
                                    xml2::as_list, xml2::read_xml)),
  xml = purrr::map(file.path(workflow_paths, "pecan.xml"),
                     purrr::compose(xml2::as_list, xml2::read_xml))
) %>%
  select(workflow_id, notes) %>%
  unnest(notes) %>%
  transmute(
    workflow_id = workflow_id,
    crown = fct_inorder(if_else(crown_model, "finite", "closed")),
    rtm = fct_inorder(if_else(multiple_scatter, "multi-scatter", "two-stream")),
    traits = fct_inorder(if_else(trait_plasticity, "plastic", "static"))
  )

wf %>%
  unnest(notes) %>%
  mutate(xml_crown = purrr::map_chr(xml, list("pecan", "model", "ed2in_tags", "CROWN_MOD", 1)),
         xml_rtm = purrr::map_chr(xml, list("pecan", "model", "ed2in_tags", "ICANRAD", 1))) %>%
  select(crown_model, xml_crown, multiple_scatter, xml_rtm)

x$pecan$model$ed2in_tags

result <- read_csv("analysis/data/derived-data/") %>%
  mutate(
    workflow_id = as.numeric(gsub("PEcAn_", "", workflows))
  )

# LAI by PFT
result %>%
  select(workflow_id, date, pft:wai_co) %>%
  mutate(pft = factor(pft)) %>%
  group_by(workflow_id, date, pft) %>%
  summarize(lai_mean = mean(lai_co),
            lai_sd = sd(lai_co),
            lai_lo = quantile(lai_co, 0.25),
            lai_hi = quantile(lai_co, 0.75)) %>%
  left_join(wf) %>%
  ggplot() +
  aes(x = date, y = lai_mean, ymin = lai_lo, ymax = lai_hi) +
  geom_ribbon(aes(color = pft, fill = pft), alpha = 0.5) +
  ## geom_line(aes(color = pft)) +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)

# Total LAI plot
result %>%
  select(workflow_id, date, pft:wai_co) %>%
  group_by(workflow_id, date, pft) %>%
  summarize(lai = sum(lai_co)) %>%
  summarize(lai_mean = mean(lai),
            lai_sd = sd(lai),
            lai_lo = quantile(lai, 0.25),
            lai_hi = quantile(lai, 0.75)) %>%
  left_join(wf) %>%
  ggplot() +
  aes(x = date, y = lai_mean, ymin = lai_lo, ymax = lai_hi) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_line() +
  facet_grid(vars(trait_plasticity), vars(crown_model, multiple_scatter),
             labeller = label_both)

result %>%
  select(workflow_id, date, pft, value = crown_area_co) %>%
  mutate(pft = factor(pft)) %>%
  group_by(workflow_id, crown, rtm, traits, date, pft) %>%
  summarize(mean = mean(value),
            lo = quantile(value, 0.25),
            hi = quantile(value, 0.75)) %>%
  ggplot() +
  aes(x = date, y = mean, ymin = lo, ymax = hi) +
  geom_ribbon(aes(fill = pft), alpha = 0.5) +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)

result %>%
  select(workflow_id, crown, rtm, traits,
         date, pft, runs, value = cb) %>%
  mutate(pft = factor(pft)) %>%
  filter(date < "1903-01-01", pft != "Pine") %>%
  ggplot() +
  aes(x = date, y = value) +
  geom_line(aes(color = pft, group = interaction(runs, pft))) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)

# Weird results for combination of finite canopy radius + two-stream RTM

# cbr_bar (relative carbon balance) explains death of early hardwood
# at the beginning of year 2.

# root_maintenance_py is very low in year 2 in that combination
# compared to others. Also because no root growth (mmean_broot_py).

# But also very little leaf biomass (mmean_bleaf_py).

# Also, declining nplant for all mid and some early hardwood.

# ALL of this seems to be because all mid (and some early) hardwoods
# start with NEGATIVE carbon balance in year 1.

##################################################
vget <- function(crown, var) {
  workflow_run_matrix("out") %>%
    left_join(workflow_structures()) %>%
    filter(crown == !!crown, rtm == "two-stream", traits == "static") %>%
    head(1) %>%
    pull(path) %>%
    fs::dir_ls(regexp = "analysis-T-1902") %>%
    ncdf4::nc_open() %>%
    ncdf4::ncvar_get(toupper(paste0("fmean_", var, "_py")))
}
mt <- function(x) if (length(dim(x)) > 1) t(x) else x
x <- purrr::map(c("closed", "finite"), vget, var = "leaf_") %>%
  setNames(c("closed", "finite")) %>%
  map(mt) %>%
  map(head, 4000)
xf <- map(x, stats::filter, filter = rep(1/48, 48), sides = 1)
xm <- do.call(cbind, x)
xmf <- do.call(cbind, xf)
matplot(xm, type = 'l', lty = "dashed")
matplot(xmf, type = 'l', lty = "solid", add = TRUE)
legend("topright", c("closed", "finite"), col = 1:2, lty = 1)
names(tnc$var) %>% tolower() %>% gsub("fmean_", "", .) %>% gsub("_py", "", .)

##################################################
# MonetDB
library(purrr)
library(magrittr)
month_file <- "analysis/data/model_output/workflows/PEcAn_99000000112/out/99000000072/analysis-E-1902-06-00-000000-g01.h5"
mnc <- ncdf4::nc_open(month_file)
mnc_vardims <- map(mnc[["var"]], "dim") %>% map(~map_chr(.x, "name"))
mnc_vardims_p <- map_chr(mnc_vardims, paste, collapse = " ") %>%
  discard(., grepl("phony_dim", names(.))) %>%
  map_chr(~gsub("phony_dim_", "", .))

writeLines(sprintf("\"%s\",", names(which(mnc_vardims_p == "0"))))

cohorts <- ncdf4::ncvar_get(mnc, "NCOHORTS_GLOBAL")
slz <- ncdf4::ncvar_get(mnc, "SLZ")

i2f <- function(i, l) {
  structure(as.integer(i), levels = levels, class = "factor")
}

setpft <- function(i) {
  stopifnot(all(i %in% c(6, 9, 10, 11)))
  pfts <- c(
    rep(NA_character_, 5),
    "Pine", NA_character_, NA_character_,
    "Early hardwood", "Mid hardwood", "Late hardwood"
  )
  ipfts <- pfts[i]
  factor(ipfts, pfts[c(9:11, 6)])
}

cohort_vars <- tibble::tribble(
  ~hdf_varname, ~variable, ~unit,
  "AGB_CO", "Aboveground biomass", "kgC ~ plant^{-1}",
  "BALIVE", "Live tissue biomass", "kgC ~ plant^{-1}",
  "BA_CO", "Basal area", "cm^2",
  "BDEAD", "Stuctural wood biomass", "kgC ~ plant^{-1}",
  "BLEAF", "Leaf biomass", "kgC ~ plant^{-1}",
  "BROOT", "Root biomass", "kgC ~ plant^{-1}",
  "BSAPWOODA", "Aboveground sapwood biomass", "kgC ~ plant^{-1}",
  "BSAPWOODB", "Belowground sapwood biomass", "kgC ~ plant^{-1}",
  "BSEEDS_CO", "Seed biomass", "kgC ~ plant^{-1}",
  "BSTORAGE", "C storage biomass", "kgC ~ plant^{-1}",
  "CBR_BAR", "Relative carbon balance", NA_character_,
  ## "CENSUS_STATUS", "Census recruit status", NA_character_, function(i) i2f(i, c("< 10cm", "new", "established")),
  "CENSUS_STATUS", "Census recruit status", NA_character_,
  "CROWN_AREA_CO", "Crown area", NA_character_,
  "DAGB_DT", "d(AGB)/dt", "kgC ~ plant^{-1} ~ year^{-1}",
  "DBA_DT", "d(Basal area)/dt", "kgC ~ plant^{-1} ~ year^{-1}",
  "DBH", "DBH", "cm",
  "DDBH_DT", "d(DBH)/dt", "cm year^{-1}",
  "DLNAGB_DT", "d(ln(AGB))/dt", "year^{-1}",
  "DLNBA_DT", "d(ln(Basal area))/dt", "year^{-1}",
  "DLNDBH_DT", "d(ln(DBH))/dt", "year^{-1}",
  "ELONGF", "Leaf drought elongation", "0 - 1",
  "HITE", "Plant height", "m",
  "KRDEPTH", "Deepest soil layer for water access", "m",# function(i) slz[i],
  "LAI_CO", "LAI", NA_character_,
  "NPLANT", "Stem density", "plants ~ m^{-2}",
  "PAW_AVG", "Plant available water", "0 - 1",
  "PFT", "PFT", NA_character_,# setpft,
  "RECRUIT_DBH", "Monthly recruit status", NA_character_,# function(i) i2f(i, c("<10cm", "new", "established")),
  "WAI_CO", "Wood area index", NA_character_
)

x <- tibble::tibble(
  date =
)

  map_dfc(cohort_vars[["hdf_varname"]], ncdf4::ncvar_get, nc = mnc) %>%
names(x)[[12]]
for (i in seq_along(x)) {
  print(i)
  print(x[[i]])
}

# Month files dimensions:
# 0 - Cohort
# 1 - Site (patch?)
# 2 - DBH class
# 3 - PFT
# 4 - Last 12 months, plus current month
# 5 - Disturbance type
# 6 - Soil layer?
# 7 - Height class?

# Month files vartypes:
# Assigned
# global(slz): 6 -- Soil depth definition (SLZ)
#
# Ignored:
# 5,5,1 -- Disturbance matrix
# 7 - Height class definition
#
# Unassigned:
# 0 -- Cohort only ---
# 1 -- Site means ---
# 3,1 -- PFT scalars ---
# 3,2,1 -- PFT x DBH class ---
# 4,0 -- Cohort averages over last 12 months
# 4,1 -- Site averages over last 12 months
# 6,1 -- Site averages for soil

t_file <- "analysis/data/model_output/workflows/PEcAn_99000000112/out/99000000072/analysis-T-1902-00-00-000000-g01.h5"
tnc <- ncdf4::nc_open(t_file)

# T file dimensions:
# 0 - Time
# 1 - Site
# 2 - Soil layer?
# 3 - DBH class
# 4 - PFT

##################################################
library(tidyverse)
library(ggplot2)
devtools::load_all(here::here(), attach_testthat = FALSE)

drake::loadd(run_params, monthly_means_site)

d <- sensitivity_inputs %>%
  ungroup() %>%
  filter(crown == crown[[1]], rtm == rtm[[1]], traits == traits[[1]])

y <- pull(d, GPP)
x <- pull(d, Early..SLA)

ssout %>%
  mutate(model = interaction(crown, rtm, traits, sep = "\n"),
         elasticity = pmax(elasticity, -200)) %>%
  separate(xvar, c("pft", "trait"), sep = "\\.\\.") %>%
  ggplot() +
  aes(x = pft, y = trait, fill = elasticity) +
  geom_tile() +
  facet_grid(vars(yvar), vars(model)) +
  scale_fill_gradient2() +
  labs(x = "PFT", y = "Trait") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = rel(0.7)))

ssout %>%
  mutate(model = interaction(crown, rtm, traits)) %>%
  separate(xvar, c("pft", "trait"), sep = "\\.\\.") %>%
  ggplot() +
  aes(x = trait, y = elasticity, fill = pft) +
  geom_col(position = position_dodge()) +
  facet_grid(vars(yvar), vars(model)) +
  scale_fill_manual(values = pfts("color")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

##################################################
# Look at BETY priors
##################################################
library(DBI)
library(RPostgres)
library(tidyverse)
library(fortebaseline)

variables <- tbl(bety(), "variables")

variables %>%
  filter(description %like% "%mort%") %>%
  select(id, name, description)

variables %>%
  filter(name %like% "%mort%") %>%
  select(id, name, description)

variables %>%
  filter(description %like% "%reflect%") %>%
  select(id, name, description)

priors <- pfts_priors()

priors %>%
  group_by(variable) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n < 4) %>%
  print(n = Inf)
##################################################
library(tidyverse)
library(fortebaseline)

f <- here::here("analysis", "data", "derived-data", "parameter-table.csv")
dat <- read_csv(f)

subset(param_table, select = -unit_parsed)

##################################################
# Get trait data using PEcAn
##################################################
library(tidyverse)
library(fs)
library(fortebaseline)

con <- bety()
DBI::dbListTables(con) %>%
  sort()

tbl(con, "modeltypes_formats") %>%
  inner_join(tbl(con, "modeltypes"), by = c("modeltype_id" = "id")) %>%
  inner_join(tbl(con, "formats"), by = c("format_id" = "id")) %>%
  glimpse()

td <- PEcAn.utils::trait.dictionary %>% as_tibble()

v <- tbl(con, "variables") %>%
  filter(name %in% !!as.character(td$id)) %>%
  collect()

pfts_priors("temperate.Early_Hardwood")
pfts_priors("temperate.Early_Hardwood")
px <- pfts_priors()


tbl(con, "priors")
tbl(con, "traits")

dat <- PEcAn.DB::get.trait.data(
  pfts = list(

  )
  pfts = c("umbs.early_hardwood", "umbs.mid_hardwood", "umbs.late_hardwood",
           "umbs.northern_pine"),
  modeltype = "ED2",
  dbfiles = here::here("analysis", "retrieved", "pecan", "dbfiles") %>%
    dir_create(),
  database = list(drv = Rpostgres::Postgres(), user = "bety", password = "bety",
                  host = "localhost", port = 7990)
)

#########################################
pfts_priors() %>%
  count(variable) %>%
  print(n = Inf)

con <- bety()
variable <- c("c2n_fineroot", "c2n_leaf")
pft <- pfts("bety_name")

trait_distribution %>%
  count(trait) %>%
  print(n = Inf)

#########################################
## a <- 1.5
## b <- 0.06
a <- 0.7
b <- 0.04

a <- 1.5
b <- 0.2
curve(dgamma(x, a, b), 0, 30)
summary(rgamma(5000, a, b))

#########################################
tbl(bety(), "variables") %>%
  filter(name %in% !!other_priors[["variable"]])

#########################################
library(data.table, mask.ok = TRUE)
library(ggplot2)
fst_file <- "~/Projects/try-raw-data/4143.fst"
dat <- fst::fst(fst_file)

try_spp <- grep("Pinus", dat$AccSpeciesName)
resp <- grep("respiration", dat$DataName)
i <- intersect(try_spp, resp)
pine_dat <- dat[i,]
setDT(pine_dat)

pine_resp <- pine_dat[
  TraitID == 54][
    !is.na(StdValue)][
      ValueKindName == "Single"]

pine_resp[, .N, .(DataName, DataID)]

ggplot(pine_resp) +
  aes(x = AccSpeciesName, y = StdValue) +
  geom_violin() +
  geom_jitter()

pine_dat[, .N, .(TraitName, TraitID)][order(N, decreasing = TRUE)]

pp <- pfts_priors(NULL, collect = FALSE)
pp %>%
  filter(pft %like% "%pine%",
         variable %like% "%leaf_respiration%") %>%
  collect() %>%
  glimpse()
priors <- tbl(bety())

species_data_sub[, .N, .(AccSpeciesName, TraitName)][order(TraitName)]

#########################################

# Look at other PEcAn-related outputs
library(fortebaseline)
library(tidyverse)

wf_dir <- "analysis/data/model_output/workflows/PEcAn_99000000112"
pft_dir <- file.path(wf_dir, "pft", "umbs.early_hardwood")

prior_dists <- file.path(pft_dir, "prior.distns.Rdata")
priors <- load_local(prior_dists)[["prior.distns"]] %>%
  as_tibble(rownames = "trait")

post_dists <- file.path(pft_dir, "post.distns.MA.Rdata")
posterior <- load_local(post_dists)[[1]] %>%
  as_tibble(rownames = "trait")

samples_file <- file.path(wf_dir, "samples.Rdata")
samples <- load_local(samples_file)
samples$trait.names
samples$ensemble.samples
str(samples, max = 1)

## priors_file <- 

## dbcon <- bety()
library(PEcAn.DB)
f <- PEcAn.DB::dbfile.check()
pftid <- tbl(dbcon, "pfts") %>% filter(name == "temperate.Early_Hardwood") %>% pull(id)
pft <- list()

dbcon <- bety()

tbl(dbcon, "dbfiles") %>%
  filter(container_type == "Posterior",
         file_name %like% "prior.distns.Rdata") %>%
  arrange(desc(created_at)) %>%
  select(file_name, created_at)
#########################################
run_params_all = read_fst(ensemble_params_file) %>%
  as_tibble()

meta_vars = run_params_all %>%
  select(-workflow_id, -path) %>%
  group_by(pft) %>%
  summarize_all(~length(unique(.x))) %>%
  tidyr::gather(variable, count, -pft, -run_id) %>%
  filter(count > 1) %>%
  distinct(variable) %>%
  pull()

workflows <- workflows %>%
  add_row(
    workflow_id = 99000000144,
    short_id = 144,
    crown_model = FALSE,
    multiple_scatter = FALSE,
    trait_plasticity = TRUE
  )

#########################################

workflow_dir <- path("analysis", "data",
                     "model_output", "workflows",
                     "PEcAn_99000000144")
run_result_dir <- path(workflow_dir, "out")
run_results_all <- dir_ls(run_result_dir)
output_dir <- run_results_all[[1]]

o_files <- dir_ls(output_dir, regexp = "analysis-I")
e_files <- dir_ls(output_dir, regexp = "analysis-E")

future::plan("multiprocess")
o_data_list <- future_map(o_files, read_i_cohort, .progress = TRUE)

o_data_df <- bind_rows(o_data_list)

o_data_df %>%
  group_by(pft, year = lubridate::floor_date(datetime, "year"),
           datetime) %>%
  summarize(value = sum(fmean_npp_co)) %>%
  filter(datetime < "1902-10-01") %>%
  ggplot() +
  aes(x = datetime, y = value, color = factor(pft)) +
  geom_line()

fname <- o_files[[1]]
var <- "radiation_profile"
var <- tolower(var)


scalar_dim <- nc[[c("var", "AREA", "dim")]][[1]][["name"]]
scalar_vars <- ncout %>%
  stringr::str_match(sprintf())

result %>%
  dplyr::left_join(meta, by = "cohort") %>%
  dplyr::select(datetime, cohort, pft, dplyr::everything())

## output_files <- dir_ls(output_dir) %>%
##   as.character() %>%
##   str_extract("analysis-[[:alpha:]]-") %>%
##   unique()

known_vars <- tribble(
  ~varname, ~ed_name, ~dimensions,
  "radiation_profile", "FMEAN_RAD_PROFILE_CO", list("radiation", "cohorts")
)

#########################################
shannon_index <- function(lai, pft) {
  total_lai <- sum(lai)
  p_i <- tapply(lai, pft, function(x) x / total_lai)
  -sum(p_i * log(p_i), na.rm = TRUE)
}

simpson_index <- function(lai, pft) {
  total_lai <- sum(lai)
  p_i <- tapply(lai, pft, function(x) x / total_lai)
  sum(p_i ^ 2, na.rm = TRUE)
}

diversity <- lai_q90 %>%
  group_by(workflow_id, run_id, year) %>%
  mutate(total_lai = sum(lai),
         p_i = lai / total_lai) %>%
  summarize(
    shannon = -sum(p_i * log(p_i)),
    simpson = sum(p_i ^ 2),
    inv_simpson = 1 / simpson
  ) %>%
  ungroup()

jja_means %>%
  filter(is.na(shannon))

diversity %>%
  gather(variable, value, shannon, simpson, inv_simpson) %>%
  ggplot() +
  aes(x = year, y = value) +
  geom_line(aes(group = run_id)) +
  facet_grid(vars(variable), vars(workflow_id))

#########################################
# Why are runs dying?
#########################################
tsplot <- function(dat) {
  ggplot(dat) +
  aes(x = year, y = value, group = param_id, color = color) +
  geom_line(alpha = 0.3) +
  scale_color_identity() +
  facet_grid(vars(variable), vars(model), scales= "free_y")
}

# Which runs are dying?
death <- jja_means %>%
  filter(year == 1915, npp < 0.5) %>%
  distinct(case, model_id)
death %>%
  count(model_id, sort = TRUE)

jja_long %>%
  semi_join(death) %>%
  filter(rtm == "two-stream", crown == "finite") %>%
  tsplot()

# How do runs look at the very beginning?
jja_long %>%
  filter(year < 1910) %>%
  tsplot()

# How do runs look in 1910?
jja_long %>%
  filter(year == 1910) %>%
  ggplot() +
  aes(x = model_id, y = value, fill = color) +
  geom_violin() +
  geom_jitter() +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_fill_identity()

# These are cases that are _really_ unproductive
# But, they don't actually die suddenly.
# What's the issue?
death <- jja_means %>%
  group_by(case) %>%
  filter(max(agb) < 0.1) %>%
  ungroup() %>%
  filter(year == 1910, agb < 0.02) %>%
  distinct(case, model_id) %>%
  mutate(param_id = as.numeric(substring(case, 0, 3)),
         low_prod = TRUE)
jja_long %>%
  semi_join(death) %>%
  filter(rtm == "two-stream", crown == "finite") %>%
  tsplot()

# What do their parameters look like?
params_long <- run_params %>%
  select(-bety_name, -pft) %>%
  gather(trait, value, -param_id, -shortname)
low_long <- params_long %>%
  left_join(select(death, param_id, low_prod)) %>%
  mutate(low_prod = if_else(is.na(low_prod), FALSE, TRUE)) %>%
  filter(!is.na(value))
ggplot(low_long) +
  aes(x = shortname, y = value, color = low_prod, fill = low_prod) +
  geom_violin(color = "black") +
  facet_wrap(vars(trait), scales = "free_y")
# No significant differences in univariate space.

params_wide <- params_long %>%
  unite(variable, shortname, trait) %>%
  spread(variable, value)
low_params <- params_wide %>%
  left_join(death) %>%
  mutate(low_prod = if_else(is.na(low_prod), FALSE, TRUE))


# Changes in variables by year
lags <- jja_means %>%
  group_by_at(vars(case, model_id, shannon:color)) %>%
  mutate_at(vars(agb:shannon), list(d = ~.x - lag(.x))) %>%
  ungroup()
lags %>%
  filter(year < 1920) %>%
  ggplot() +
  aes(x = year, y = lai_d, color = color, group = case) +
  geom_line(alpha = 0.3) +
  facet_grid(cols = vars(model))

death2 <- lags %>%
  filter(year < 1920, lai_d < -2) %>%
  distinct(case, model_id)
count(death2, model_id, sort = TRUE)
jja_long %>%
  semi_join(death2) %>%
  tsplot()


f <- fst(cohort_file)
ddeath <- f[f$case %in% death$case & f$datetime < "1911-01-01",]
setDT(ddeath)

names(f)

dat <- f[f$datetime < "1904-01-01", ] %>%
  setDT() %>%
  .[, lp := case %in% death$case] %>%
  .[, casefile := NULL] %>%
  ## .[, pft := NULL] %>%
  ## .[, lapply(.SD, mean), .(case, datetime, lp)] %>%
  .[, case := forcats::fct_reorder(case, lp)] %>%
  .[, model_id := substring(case, 4, 6)]

# Interesting variables:
# fmean_a_net_co -- Actual assimilation
# fmean_a_light_co -- Light-limited assimilation.
# lai_co -- Leaf area
ggplot(dat[datetime < "1902-06-05",]) +
  aes(x = datetime, y = bleaf,
      color = lp, alpha = lp,
      group = interaction(case, pft)) +
  geom_line() +
  facet_grid(vars(pft), vars(model_id)) +
  scale_color_manual(values = c("TRUE" = "dark red", "FALSE" = "grey80")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2))

ddeath %>%
  .[datetime < "1904-01-01"] %>%
  .[, .(case, datetime, nplant)] %>%
  .[, .(nplant = sum(nplant)), .(case, datetime)] %>%
  ggplot() +
  aes(x = datetime, y = nplant) +
  geom_line() +
  facet_wrap(vars(case))

ggplot(ddeath) +
  aes(x = datetime, y = dbh, group)

#########################################
hff <- "analysis/data/model_output/workflows/PEcAn_99000000144/out/99000000171/analysis-I-1902-06-03-000000-g01.h5"
hf <- hdf5r::H5File$new(hff, "r")
names(hf)[7*10 + 1:10]
hf[[names(hf)[34]]][]
hf[["LEAF_DROP_PY"]][,,]
hf$close_all()

nc <- tidync::tidync(hff)
nc$grid

v <- grep("_PY$", names(hf), value = TRUE)
for (i in v) print(hf[[i]])

#########################################
# Which runs suddenly dying?
d <- jja_means
setDT(d)
sdcols <- c("agb", "gpp", "npp", "lai")
dvar <- d[order(year, case, model_id),] %>%
  .[, `:=`(dagb = agb - shift(agb),
           dgpp = gpp - shift(gpp),
           dnpp = npp - shift(npp),
           dlai = lai - shift(lai)), .(case, model_id)]

dsub <- dvar[year > 1915][agb < 0.1][dagb < -0.4]
dsub <- dvar[year > 1915][lai < 0.1][dlai < -3]
ddied <- unique(dsub[, .(case)])
d_deathyear <- unique(dsub[, .(case, year)])

ddied[d, on = "case", nomatch = 0] %>%
  ggplot() +
  aes(x = year, y = agb) +
  geom_line() +
  facet_wrap(vars(case))

ccc <- c("034CMP", "034FMS", "110FTP", "110FTS", "153FTS",
         "009FMP", "012CMP", "012CTP", "012CTS")

xf <- fst(cohort_file)
x <- xf[xf$datetime < "1950-01-01" &
          xf$datetime > "1947-01-01" &
          grepl("034|110|153", xf$case),]
setDT(x)

x[, .(y = sum(fmean_fsw_co * nplant)), .(case, datetime, pft)] %>%
  .[, param_id := substring(case, 0, 3)] %>%
  ggplot() +
  aes(x = datetime, y = y,
      color = factor(param_id),
      linetype = factor(pft),
      group = interaction(case, pft)) +
  geom_line() +
  scale_x_datetime(date_breaks = "2 months")

plong <- run_params %>%
  gather(variable, value, -param_id, -bety_name, -pft, -shortname)
psub <- filter(plong, param_id %in% c(34, 110, 153))
ggplot() +
  aes(x = shortname, y = value, fill = shortname) +
  geom_violin(data = plong, fill = "grey70") +
  geom_point(aes(color = factor(param_id)), data = psub, size = 2) +
  facet_wrap(vars(variable), scales = "free_y")

# In 1948, some runs are dying because of C starvation triggered by
# water stress (fmean_fsw_co). Sensitivity to drought is caused by
# stomatal conductance -- high stomatal conductance. The high
# mortality may also be related to the high value of `mort1`
# (background mortality rate).

# Was 1948 a particularly dry year?
sf <- fst("analysis/data/retrieved/all-output-soil.fst")
s <- sf[sf$param_id %in% c(34, 110, 153), ]
setDT(s)

s %>%
  .[slz >= -0.7, lapply(.SD, mean),
    .(case, floor_date(datetime, "month")),
    .SDcols = map_lgl(s, is.numeric)] %>%
  .[floor_date > "1940-01-01" & floor_date < "1950-01-01"] %>%
  ggplot() +
  aes(x = floor_date, y = fmean_soil_water_py, color = factor(param_id), group = case) +
  geom_line() +
  scale_x_datetime(date_breaks = "6 months")

scf <- fst("analysis/data/retrieved/all-output-scalar.fst")
scf_cols <- c("fmean_atm_temp_py", "fmean_atm_vpdef_py",
              "total_agb_mort", "total_basal_area_mort")
sc <- scf[scf$param_id %in% c(34, 110, 153),
          c("datetime", "case", "param_id", scf_cols)]
setDT(sc)

summary(sc)

sc %>%
  .[datetime > "1940-01-01" & datetime < "1950-01-01"] %>%
  .[, lapply(.SD, mean), .(param_id, case, floor_date(datetime, "month")),
    .SDcols = scf_cols] %>%
  ggplot() +
  aes(x = floor_date, y = total_basal_area_mort,
      color = factor(param_id), group = case) +
  geom_line()

pyf <- fst("analysis/data/retrieved/all-output-pft.fst")

#########################################
# Why are all runs stopping in 1977?
# First guess: Met
library(tidync)
nc <- tidync("analysis/data/retrieved/CUSTOM_ED2_site_1-33/1978JAN.h5")
met <- hyper_tibble(nc)

#########################################
ma_prior %>%
  filter(shortname == "Early", trait == "root_respiration_rate")

params_raw %>%
  filter(trait == "root_respiration_rate")

meta_analysis_file <- "analysis/data/retrieved/meta-analysis.rds"
ma <- readRDS(meta_analysis_file)
ma_eh <- ma[["Early hardwood"]]

ma_prior %>%
  filter(trait == "water_conductance")

x <- rlnorm(5000, -5.4, 3)
curve(log10(qlnorm(x, -5.4, 3)))
qlnorm(c(0.025, 0.25, 0.5, 0.75, 0.975), log(2e-5), 3.5)
log10(qlnorm(c(0.005, 0.995), exp(-5.4)-, 3))
1 - plnorm(, -5.4, 3)
qlnorm(-5.4, 3)
summary(x)
hist(x)

con <- bety()

wid <- 45
tbl(con, "priors") %>%
  filter(variable_id == wid) %>%
  collect() %>%
  glimpse()
#########################################
library(data.table)
library(fst)
library(magrittr)
library(ggplot2)

f <- fst("analysis/data/retrieved/all-output-monthly-scalar.fst")
nf <- names(f)
vvv <- c("case", "model_id", "param_id", "datetime",
         grep("^mmean_(npp|gpp|nep)", nf, value = TRUE))
d <- setDT(f[, vvv])
# Assign datetimes by hand
mseq <- function(i) {
  j <- i + 5
  m <- j %% 12
  m[m == 0] <- 12
  y <- 1902 + j %/% 12
  ISOdate(y, m, 01, tz = "UTC")
}
d <- d[, datetime := mseq(seq_len(.N)), case]

ymeans <- d %>%
  .[month(datetime) %in% c(6, 8),] %>%
  .[, yr := year(datetime)] %>%
  .[, lapply(.SD, mean), .(case, model_id, param_id, yr)]

ggplot(ymeans) +
  aes(x = yr, y = mmean_gpp_py, group = case) +
  geom_line(alpha = 0.5) +
  facet_wrap(vars(model_id), scales = "fixed")

means <- d[datetime > "1950-01-01" & month(datetime) %in% c(6, 8),
           lapply(.SD, mean),
           .(case, model_id, param_id)]
hist(means[, mmean_npp_py])

#########################################
# Read monthly cohort output

f <- fst("analysis/data/retrieved/all-output-monthly-cohort.fst")
nf <- names(f)
vvv <- c("case", "model_id", "param_id", "datetime", "pft", "nplant",
         "agb_co", "balive", "bdead", "bleaf", "broot",
         "bsapwooda", "bsapwoodb", "bstorage", "dbh", "lai_co", 
         grep("^mmean_(npp|gpp)", nf, value = TRUE))
d <- setDT(f[, vvv])
# Assign datetimes by hand
mseq <- function(i) {
  j <- i + 5
  m <- j %% 12
  m[m == 0] <- 12
  y <- 1902 + j %/% 12
  ISOdate(y, m, 01, tz = "UTC")
}
d <- d[, datetime := mseq(seq_len(.N)), .(case, pft, dbh)]

ggplot(d) +
  aes(x = factor(pft), y = nplant) +
  geom_violin() +
  facet_wrap(vars(model_id), scales = "fixed")

summary(d)

ymeans <- d %>%
  .[month(datetime) %in% c(6, 8),] %>%
  .[, yr := year(datetime)] %>%
  .[, lapply(.SD, mean), .(case, model_id, param_id, yr)]

ggplot(ymeans) +
  aes(x = yr, y = mmean_gpp_py, group = case) +
  geom_line(alpha = 0.5) +
  facet_wrap(vars(model_id), scales = "fixed")

#########################################
# Cleaner sensitivity results figure
#########################################

sensitivity_plot_data
