library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

out_root <- dir_create(here("analysis", "data", "retrieved", "ed-tests"))
ed_input_dir <- path_home("Projects", "forte_project", "umbs-ed-inputs")

future::plan(future.callr::callr())

param_draws <- read_csv("analysis/data/retrieved/input-parameters.csv")

soil_data <- read.csv(here::here(
  "analysis",
  "data",
  "derived-data",
  "soil-moisture.csv"
), header = TRUE, stringsAsFactors = FALSE) %>%
  # Make depth negative
  dplyr::mutate(depth = -depth) %>%
  # Start with deepest layer
  dplyr::arrange(depth)

ed2in_common <- PEcAn.ED2::read_ed2in(here("inst", "ED2IN")) %>%
  modifyList(list(
    # Start and end date
    POI_LAT = 45.5625, POI_LON = -84.6975,
    VEG_DATABASE = path(ed_input_dir, "EDI", "oge2OLD", "OGE2_"),
    SOIL_DATABASE = path(ed_input_dir, "EDI", "faoOLD", "FAO_"),
    LU_DATABASE = path(ed_input_dir, "EDI", "ed_inputs", "glu"),
    THSUMS_DATABASE = file.path(ed_input_dir, "EDI", "ed_inputs/"),
    ED_MET_DRIVER_DB = path(
      ed_input_dir,
      "met",
      "CUSTOM_ED2_site_1-33",
      "ED_MET_DRIVER_HEADER"
    ),
    OBSTIME_DB = file.path(ed_input_dir, "forte_obstime.time"),
    # Enable "observed" fast output at specified interval
    IOOUTPUT = 3,
    IMOUTPUT = 3,
    # Include monthly history files
    ISOUTPUT = 3,
    UNITSTATE = 2,
    FRQSTATE = 1,
    # Disable other outputs
    ITOUTPUT = 0, IFOUTPUT = 0, IDOUTPUT = 0, IQOUTPUT = 0, IYOUTPUT = 0, OUTFAST = 0,
    INCLUDE_THESE_PFT = c(6, 9, 10, 11),
    ISOILFLG = 2, # Set soil characteristics in ED2IN
    # UMBS soil characteristics (from Gough et al. 2010 FEM)
    NSLCON = 1, # Sand
    SLXCLAY = 0.01,
    SLXSAND = 0.92,
    NZG = nrow(soil_data),
    SLZ = soil_data[["depth"]],
    SLMSTR = soil_data[["slmstr"]],
    IADD_COHORT_MEANS = 1,
    PLANT_HYDRO_SCHEME = 0,
    ISTOMATA_SCHEME = 0,
    ISTRUCT_GROWTH_SCHEME = 0,
    INTEGRATION_SCHEME = 3,
    RADFRQ = 900,
    DTLSM = 900,
    RK4_TOLERANCE = 0.01
  ))

### Custom simulations
run_ed <- function(casename,
                   trait_values = list(),
                   start_date = "1902-06-01",
                   end_date = "1920-06-01",
                   trait_plasticity = FALSE,
                   multiple_scatter = FALSE,
                   crown_model = FALSE,
                   water_lim = TRUE,
                   ...) {

  start_date <- as.POSIXct(start_date, tz = "UTC")
  end_date <- as.POSIXct(end_date, tz = "UTC")

  outdir <- dir_create(path(out_root, casename))
  xmlfile <- path(outdir, "config.xml")
  ed2infile <- path(outdir, "ED2IN")

  tv_default <- list(
    umbs.early_hardwood = list(),
    umbs.mid_hardwood = list(),
    umbs.late_hardwood = list(),
    umbs.northern_pine = list()
  )
  tv_list <- modifyList(tv_default, trait_values)
  config_xml <- write_ed2_xml(tv_list)
  XML::saveXML(config_xml, xmlfile)

  ed2in <- modifyList(
    ed2in_common,
    list(
      IYEARA = lubridate::year(start_date),
      IMONTHA = lubridate::month(start_date),
      IDATEA = lubridate::mday(start_date),
      IYEARZ = lubridate::year(end_date),
      IMONTHZ = lubridate::month(end_date),
      IDATEZ = lubridate::mday(end_date),
      FFILOUT = path(outdir, "analysis"),
      SFILOUT = path(outdir, "history"),
      IEDCNFGF = xmlfile,
      TRAIT_PLASTICITY_SCHEME = as.integer(trait_plasticity),
      ICANRAD = ifelse(multiple_scatter, 1, 2),
      CROWN_MOD = as.integer(crown_model),
      H2O_PLANT_LIM = as.integer(water_lim),
      ...
    )
  )

  PEcAn.ED2::write_ed2in(ed2in, ed2infile, barebones = TRUE)

  # Run ED
  std_out <- path(outdir, "stdout.log")
  std_err <- path(outdir, "stderr.log")
  processx::process$new("ed2-opt", c("-f", ed2infile),
                        stdout = std_out, stderr = std_err)
}

read_output_dir <- function(outdir) {
  efiles <- fs::dir_ls(outdir, glob = "*/analysis-E-*")
  e_data_list <- furrr::future_map(efiles, read_e_file, .progress = TRUE)
  result_dfs <- list(
    scalar = purrr::map_dfr(e_data_list, "scalar"),
    cohort = purrr::map_dfr(e_data_list, "cohort"),
    soil = purrr::map_dfr(e_data_list, "soil"),
    pft = purrr::map_dfr(e_data_list, "pft")
  )
  result_dfs
}

ptest <- run_ed("tout", end_date = "1910-01-01", IOOUTPUT = 0, ITOUTPUT = 3)
tail(readLines(path(out_root, "tout", "stdout.log")))

mout <- read_output_dir(path(out_root, "tout"))

mout$scalar %>%
  ggplot() +
  aes(x = datetime, y = mmean_npp_py) +
  geom_line()

tfiles <- fs::dir_ls(path(out_root, "tout"), glob = "*/analysis-T-*")
mfiles <- fs::dir_ls(path(out_root, "tout"), glob = "*/analysis-E-*")

tnc <- ncdf4::nc_open(tfiles[[1]])
gpp <- ncdf4::ncvar_get(tnc, "FMEAN_GPP_PY")

sum(ncdf4::ncvar_get(tnc, "FMEAN_GPP_PY"))
sum(ncdf4::ncvar_get(tnc, "FMEAN_PLRESP_PY"))

mnc <- ncdf4::nc_open(mfiles[[1]])
ncdf4::ncvar_get(mnc, "MMEAN_GPP_PY") * 48 * 30
ncdf4::ncvar_get(mnc, "MMEAN_PLRESP_PY")

pdefault <- run_ed("default2", end_date = "1949-12-31")
pnowater <- run_ed("nowater", end_date = "1949-12-31", water_lim = FALSE)
pdefsoil <- run_ed("defsoil", end_date = "1949-12-31",
                   SLMSTR = rep(1.0, nrow(soil_data)))
pclay <- run_ed("clay", end_date = "1949-12-31",
                SLXCLAY = 0.68, SLXSAND = 0.20)

tail(readLines(path(out_root, "clay", "stdout.log")))

out_default <- read_output_dir(path(out_root, "default2"))
out_nowater <- read_output_dir(path(out_root, "nowater"))
out_defsoil <- read_output_dir(path(out_root, "defsoil"))
out_clay <- read_output_dir(path(out_root, "clay"))

scalar <- bind_rows(
  default = out_default$scalar,
  no_h2o_lim = out_nowater$scalar,
  slmstr = out_defsoil$scalar,
  clay = out_clay$scalar,
  .id = "config"
)

get_gs <- . %>%
  select(-case, -model_id, -param_id) %>%
  filter(lubridate::month(datetime) %in% 6:8) %>%
  mutate(year = lubridate::year(datetime)) %>%
  select(-datetime)

gs_mean <- scalar %>%
  get_gs %>%
  group_by(config, year) %>%
  summarize_all(mean) %>%
  ungroup()

dpft <- bind_rows(
  default = out_default$pft,
  no_h2o_lim = out_nowater$pft,
  slmstr = out_defsoil$pft,
  clay = out_clay$pft,
  .id = "config"
)

dpft_gs <- dpft %>%
  get_gs %>%
  group_by(config, year, pft) %>%
  summarize_all(mean) %>%
  ungroup()

dpft_gs %>%
  select(year, config, pft,
         basal_area_py, mmean_bleaf_py, mmean_broot_py, mmean_bstorage_py,
         mmean_lai_py, agb_py, nplant_py) %>%
  pivot_longer(-c(year, config, pft)) %>%
  ggplot() +
  aes(x = year, y = value, color = factor(pft), linetype = config) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  facet_wrap(vars(name), scales = "free_y")

lai <- dpft_gs %>%
  group_by(config, year) %>%
  summarize_at(
    vars(mmean_lai_py, nplant_py),
    sum
  ) %>%
  ungroup()

gs_mean %>%
  left_join(lai, c("config", "year")) %>%
  select(year, config,
         mmean_lai_py, nplant_py,
         starts_with("mmean_a_"),
         matches("[gn]pp"), matches("resp"), matches("_rh_")) %>%
  pivot_longer(-c(year, config)) %>%
  ggplot() +
  aes(x = year, y = value, color = config) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y")

ggplot(gs_mean) +
  aes(x = year, y = mmean_a_closed_py, color = config) +
  geom_line()

vcols <- out_default$scalar %>%
  summarize_all(n_distinct) %>%
  pivot_longer(everything()) %>%
  filter(
    value > 1,
    !(name %in% c("age", "mmean_qthroughfall_py", "mmean_throughfall_py"))
  ) %>%
  pull(name)

scalar %>%
  select(config, !!vcols) %>%
  pivot_longer(-c(datetime, config), "variable") %>%
  ggplot() +
  aes(x = datetime, y = value, color = config) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")
