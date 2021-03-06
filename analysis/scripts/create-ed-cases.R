library(fortebaseline)
library(tidyverse)

pic_input_dir <- file.path("analysis", "data", "model_output", "pic-inputs")
dir.create(pic_input_dir, recursive = TRUE, showWarnings = FALSE)

param_draws <- read_csv("analysis/data/retrieved/input-parameters.csv")
nparams <- nrow(param_draws)

create_case <- function(case,
                        remote_basedir = file.path("/qfs", "projects", "forteproject"),
                        local_basedir = remote_basedir) {
  casename <- sprintf("%03d%s", case$param_id, case$case)
  outdir <- file.path(local_basedir, casename)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  config_xml <- as_pft_list(case$data) %>% write_ed2_xml()
  XML::saveXML(config_xml, file.path(outdir, "config.xml"))
  ed2in_template_file <- file.path("inst", "ED2IN")
  ed2in_template <- PEcAn.ED2::read_ed2in(ed2in_template_file)
  remote_input_dir <- file.path(remote_basedir, "ed")
  remote_output_dir <- file.path(remote_basedir, "forte-ed-runs")

  soil_data <- umbs_soil()

  ed2in_tags <- list(
    # Start and end date
    IYEARA = 1902, IMONTHA = 6, IDATEA = 1,
    IYEARZ = 2000, IMONTHZ = 1, IDATEZ = 1,
    # Site
    POI_LAT = 45.5625, POI_LON = -84.6975,
    # Outputs
    FFILOUT = file.path(remote_output_dir, "cases", casename, "analysis"),
    SFILOUT = file.path(remote_output_dir, "cases", casename, "history"),
    # Inputs
    IEDCNFGF = file.path(remote_output_dir, "cases", casename, "config.xml"),
    VEG_DATABASE = file.path(remote_input_dir, "EDI", "oge2OLD", "OGE2_"),
    SOIL_DATABASE = file.path(remote_input_dir, "EDI", "faoOLD", "FAO_"),
    LU_DATABASE = file.path(remote_input_dir, "EDI", "ed_inputs", "glu"),
    THSUMS_DATABASE = file.path(remote_input_dir, "EDI", "ed_inputs/"),
    ED_MET_DRIVER_DB = file.path(
      remote_input_dir,
      "met",
      "CUSTOM_ED2_site_1-33",
      "ED_MET_DRIVER_HEADER"
    ),
    # No tower output -- this makes runs 10-20x faster
    ITOUTPUT = 0,
    # Monthly output instead
    IMOUTPUT = 3,
    IQOUTPUT = 3,
    IYOUTPUT = 3,
    # Disable "observed" fast output at specified interval
    IOOUTPUT = 0,
    OBSTIME_DB = "",
    OUTFAST = 0,
    # Include monthly history files
    ISOUTPUT = 3,
    UNITSTATE = 2,
    FRQSTATE = 1,
    # Other outputs
    IFOUTPUT = 0,
    IDOUTPUT = 0,
    # Enable cohort-level output
    IADD_COHORT_MEANS = 1,
    PLANT_HYDRO_SCHEME = 0,
    ISTOMATA_SCHEME = 0,
    ISTRUCT_GROWTH_SCHEME = 0,
    TRAIT_PLASTICITY_SCHEME = as.integer(case$trait_plasticity),
    ICANRAD = ifelse(case$multiple_scatter, 1, 2),
    CROWN_MOD = as.integer(case$crown_model),
    INTEGRATION_SCHEME = 3,
    RADFRQ = 900,
    DTLSM = 900,
    RK4_TOLERANCE = 0.01,
    ## N_PLANT_LIM = 0,
    ## N_DECOMP_LIM = 0,
    INCLUDE_THESE_PFT = c(6, 9, 10, 11),
    ISOILFLG = 2, # Set soil characteristics in ED2IN
    # UMBS soil characteristics (from Gough et al. 2010 FEM)
    NSLCON = 1, # Sand
    SLXCLAY = 0.01,
    SLXSAND = 0.92,
    # Soil moisture data from Ameriflux
    # See analysis/scripts/soil-moisture.R
    NZG = nrow(soil_data),
    SLZ = soil_data[["depth"]],
    SLMSTR = soil_data[["slmstr"]]
  )
  ed2in <- modifyList(ed2in_template, ed2in_tags)
  PEcAn.ED2::write_ed2in(ed2in, file.path(outdir, "ED2IN"), barebones = TRUE)
  invisible(outdir)
}

param_nest <- param_draws %>%
  nest(-param_id)

ed2in_template_file <- file.path("inst", "ED2IN")
ed2in_template <- PEcAn.ED2::read_ed2in(ed2in_template_file)

structures <- tibble(
  crown_model = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  trait_plasticity = c(TRUE, FALSE)
) %>%
  expand(., !!!(syms(colnames(.)))) %>%
  mutate(case = paste0(
    if_else(crown_model, "F", "C"),
    if_else(multiple_scatter, "M", "T"),
    if_else(trait_plasticity, "P", "S")
  )) %>%
  select(case, everything())

cases <- crossing(structures, param_nest)
PEcAn.logger::logger.setLevel("INFO")
pb <- progress::progress_bar$new(total = nrow(cases))
for (c in purrr::transpose(cases)) {
  pb$tick()
  tryCatch(
    create_case(c),
    error = function(e) {
      message("Failed with error:\n",
              conditionMessage(e))
    }
  )
}
