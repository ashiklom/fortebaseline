#' Start an ED2 model run using `processx`
#'
#' @param casename Name
#' @param trait_values Named list of custom parameters
#' @param start_date,end_date Run start and end dates. Default is 1902-06-01 to
#'   1920-01-01.
#' @param trait_plasticity Whether or not to enable the trait
#'   plasticity scheme (default = `FALSE`)
#' @param multiple_scatter Whether or not to use the
#'   multiple-scattering canopy RTM. If `FALSE` (default), use the
#'   two-stream RTM.
#' @param crown_model Whether or not to use the finite canopy radius
#'   model (default = `FALSE`)
#' @param water_lim Whether or not to run with water limitation to
#'   photosynthesis (default = `TRUE`).
#' @param out_root Path to ED2 output root directory. Default is the option
#'   `fortebaseline.ed_root`, and if unset, "ed2-output" in the project root
#'   directory.
#' @param ... Additional ED2IN namelist settings. Must be in all caps, and must
#'   match settings in ED2IN.
#' @return `processx` process for ED2 run.
#' @author Alexey Shiklomanov
#' @export
run_ed <- function(casename,
                   trait_values = list(),
                   start_date = "1902-06-01",
                   end_date = "1920-01-01",
                   trait_plasticity = FALSE,
                   multiple_scatter = FALSE,
                   crown_model = FALSE,
                   water_lim = TRUE,
                   out_root = getOption("fortebaseline.ed_root"),
                   ed_exe = fs::path(getOption("fortebaseline.ed_src_dir"),
                                     "ED", "build", "ed_2.1-opt"),
                   ...) {

  stopifnot(
    !is.null(ed_exe) &&
      length(ed_exe) > 0 &&
      fs::file_exists(ed_exe)
  )
  start_date <- as.POSIXct(start_date, tz = "UTC")
  end_date <- as.POSIXct(end_date, tz = "UTC")

  if (is.null(out_root)) out_root <- here::here("ed2-output")
  outdir <- fs::dir_create(fs::path(out_root, casename))
  xmlfile <- fs::path(outdir, "config.xml")
  ed2infile <- fs::path(outdir, "ED2IN")

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
    ed2in_common(),
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
  std_out <- fs::path(outdir, "stdout.log")
  std_err <- fs::path(outdir, "stdout.log")
  processx::process$new(
    system.file("ed2-exe.sh", package = "fortebaseline"),
    c(ed_exe, ed2infile),
    stdout = std_out,
    stderr = std_err,
    post_process = function() read_efile_dir(outdir)
  )
}

#' Run ED2, but only if there isn't a run there already
#'
#' @param overwrite (Logical) If `TRUE`, delete the target output directory and
#'   force a run.
#' @inheritParams run_ed
#' @return List containing the output directory (`outdir`), the
#'   `processx::process` object (`p`), and a function for checking the current
#'   log status (`log`).
#' @export
run_ed_maybe <- function(casename,
                         overwrite = FALSE,
                         out_root = getOption("fortebaseline.ed_root"),
                         ...) {
  outdir <- fs::path(out_root, casename)
  logfun <- function(...) tail_ed_output(
    casename = casename,
    out_root = out_root,
    ...
  )
  if (overwrite) {
    message("Force-removing old files.")
    fs::dir_delete(outdir)
  }
  if (fs::dir_exists(outdir) &&
        length(fs::dir_ls(outdir, regexp = "analysis-")) > 0) {
    message("Existing files found. Skipping run.")
    proc <- NULL
  } else {
    proc <- run_ed(casename = casename, ...)
  }
  list(outdir = outdir, p = proc, log = logfun)
}

#' View the end of the ED2 output log file
#'
#' @param ... Additional arguments to [utils::tail()]
#' @inheritParams run_ed
#' @export
tail_ed_output <- function(casename,
                           out_root = getOption("fortebaseline.ed_root"),
                           ...) {
  logfile <- fs::path(out_root, casename, "stdout.log")
  tail(readLines(logfile), ...)
}

#' Directory containing ED2 inputs
ed_input_dir <- function() {
  out <- getOption("fortebaseline.ed_input_dir")
  stopifnot(!is.null(out), file.exists(out))
  out
}

#' Common ED2IN settings for FoRTE baseline runs
ed2in_common <- function() {
  soil_data <- umbs_soil()
  system.file("ED2IN", package = "fortebaseline") %>%
    PEcAn.ED2::read_ed2in() %>%
    modifyList(list(
      # Start and end date
      POI_LAT = 45.5625, POI_LON = -84.6975,
      VEG_DATABASE = path(ed_input_dir(), "EDI", "oge2OLD", "OGE2_"),
      SOIL_DATABASE = path(ed_input_dir(), "EDI", "faoOLD", "FAO_"),
      LU_DATABASE = path(ed_input_dir(), "EDI", "ed_inputs", "glu"),
      THSUMS_DATABASE = file.path(ed_input_dir(), "EDI", "ed_inputs/"),
      ED_MET_DRIVER_DB = path(
        ed_input_dir(),
        "met",
        "CUSTOM_ED2_site_1-33",
        "ED_MET_DRIVER_HEADER"
      ),
      OBSTIME_DB = file.path(ed_input_dir(), "forte_obstime.time"),
      # Disable "observed" fast output at specified interval. It interferes with monthly.
      IOOUTPUT = 0,
      IMOUTPUT = 3,
      IQOUTPUT = 3,
      IYOUTPUT = 3,
      # Include monthly history files
      ISOUTPUT = 3,
      UNITSTATE = 2,
      FRQSTATE = 1,
      # Disable fast outputs
      ITOUTPUT = 0,
      IFOUTPUT = 0,
      IDOUTPUT = 0,
      OUTFAST = 0,
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
}
