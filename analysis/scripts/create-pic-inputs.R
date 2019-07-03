library(fortebaseline)
library(tidyverse)

pic_input_dir <- file.path("analysis", "data", "model_output", "pic-inputs")
dir.create(pic_input_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(12345678)

nparams <- 200

write_ed2_xml <- function(trait_values) {
  pft_list <- tibble::tibble(bety_name = names(trait_values)) %>%
    dplyr::left_join(pfts(), by = "bety_name") %>%
    dplyr::select(name = bety_name, ed2_pft_number = num) %>%
    purrr::transpose()
  settings <- list(
    model = list(revision = "git"),
    pfts = pft_list
  )
  PEcAn.ED2::write.config.xml.ED2(settings, trait_values)
}

as_pft_list <- function(dat) {
  names <- dat[["name"]]
  stopifnot(!is.null(names))
  vals <- dat %>%
    dplyr::select(-name) %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::map(purrr::discard, is.na)
  names(vals) <- names
  vals
}

create_case <- function(case, basedir = "analysis/data/model_output/cases") {
  casename <- sprintf("%03d%s", case$param_id, case$case)
  outdir <- file.path(basedir, casename)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  config_xml <- as_pft_list(case$data) %>% write_ed2_xml()
  XML::saveXML(config_xml, file.path(outdir, "config.xml"))
  ed2in_template_file <- file.path("inst", "ED2IN")
  ed2in_template <- PEcAn.ED2::read_ed2in(ed2in_template_file)
  remote_basedir <- file.path("/projects", "birthright", "shik544")
  remote_input_dir <- file.path(remote_basedir, "ed")
  remote_output_dir <- file.path(remote_basedir, "forte-ed-runs")

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
    ED_MET_DRIVER_DB = file.path(remote_input_dir, "met",
                                 "CUSTOM_ED2_site_1-33", "ED_MET_DRIVER_HEADER"),
    # No tower output -- this makes runs 10-20x faster
    ITOUTPUT = 0,
    # Monthly output instead
    IMOUTPUT = 3,
    # Enable "observed" fast output at specified interval
    IOOUTPUT = 3,
    OBSTIME_DB = file.path(remote_output_dir, "forte_obstime.time"),
    OUTFAST = 0,
    # Include monthly history files
    ISOUTPUT = 3,
    FRQSTATE = 1,
    UNITSTATE = 2,
    # Other outputs
    IFOUTPUT = 0,
    IDOUTPUT = 0,
    IQOUTPUT = 0,
    IYOUTPUT = 0,
    # Enable cohort-level output
    IADD_COHORT_MEANS = 1,
    PLANT_HYDRO_SCHEME = 0,
    ISTOMATA_SCHEME = 0,
    ISTRUCT_GROWTH_SCHEME = 0,
    TRAIT_PLASTICITY_SCHEME = as.integer(case$trait_plasticity),
    ICANRAD = ifelse(case$multiple_scatter, 1, 2),
    CROWN_MOD = as.integer(case$crown_model),
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

params_raw <- drake::readd("trait_distribution")
params <- params_raw %>%
  select(-num, -bety_name) %>%
  left_join(params_raw %>%
              distinct(pft, num, bety_name) %>%
              filter(!is.na(num)), "pft") %>%
  unnest(draws) %>%
  filter(trait != "cuticular_cond") %>%
  select(name = bety_name, trait, draws)

param_draws <- params %>%
  group_by(name, trait) %>%
  sample_n(nparams) %>%
  mutate(param_id = row_number()) %>%
  spread(trait, draws) %>%
  ungroup() %>%
  select(param_id, everything())

write_csv(param_draws, "analysis/data/retrieved/input-parameters.csv")

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
pb <- progress::progress_bar$new(total = length(cases))
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
