#' Run an ED ensemble using PEcAn
#'
#' @param start_date First day of run
#' @param end_date Last day of run
#' @param ensemble_size Number of ensemble members. Default = 1.
#' @param pft_type PFT definition. Either "umbs" (default) or
#'   "standard".
#' @param con PEcAn database connection. If `NULL` (default), create a
#'   connection using default settings.
#' @param nowait Logical. If `TRUE` (default), tell PEcAn not to wait
#'   for models to finish running before starting another workflow.
#' @param crown_model Whether or not to use the finite canopy radius
#'   model (default = `FALSE`)
#' @param n_limit_ps Whether or not photosynthesis is N-limited
#'   (default = `FALSE`)
#' @param n_limit_soil Whether or not soil respiration is N-limited
#'   (default = `FALSE`)
#' @param multiple_scatter Whether or not to use the
#'   multiple-scattering canopy RTM. If `FALSE` (default), use the
#'   two-stream RTM.
#' @param trait_plasticity Whether or not to enable the trait
#'   plasticity scheme (default = `FALSE`)
#' @param ... Additional modifications to the settings, passed to
#'   [utils::modifyList()]
#' @return List containing the workflow ID (`workflow_id`) and the
#'   full settings list object (`settings`)
#' @author Alexey Shiklomanov
#' @export
run_ed_ensemble <- function(start_date, end_date,
                            ensemble_size = 1,
                            pft_type = "umbs",
                            con = NULL,
                            nowait = TRUE,
                            crown_model = FALSE,
                            n_limit_ps = FALSE,
                            n_limit_soil = FALSE,
                            multiple_scatter = FALSE,
                            trait_plasticity = FALSE,
                            ...) {

  note_string <- paste(
    "==FoRTE run==",
    "crown_model : %s",
    "n_limit_ps : %s",
    "n_limit_soil : %s",
    "multiple_scatter : %s",
    "trait_plasticity : %s",
    sep = "\n"
  )
  notes <- sprintf(
    note_string,
    crown_model,
    n_limit_ps,
    n_limit_soil,
    multiple_scatter,
    trait_plasticity
  )

  pft_type <- tolower(pft_type)
  stopifnot(pft_type %in% c("umbs", "standard"))

  pft_list <- switch(
    pft_type,
    umbs = list(
      list(name = "umbs.early_hardwood", ed2_pft_number = 9),
      list(name = "umbs.mid_hardwood", ed2_pft_number = 10),
      list(name = "umbs.late_hardwood", ed2_pft_number = 11),
      list(name = "umbs.northern_pine", ed2_pft_number = 6)
    ),
    standard = list(
      list(name = "temperate.Early_Hardwood", ed2_pft_number = 9),
      list(name = "temperate.North_Mid_Hardwood", ed2_pft_number = 10),
      list(name = "temperate.Late_Hardwood", ed2_pft_number = 11),
      list(name = "temperate.Northern_Pine", ed2_pft_number = 6)
    )
  )

  if (is.null(con)) con <- default_connection()

  model_id <- pecanapi::get_model_id(con, "ED2-experimental", "experimental")
  site_id <- 1000000033                   # UMBS disturbance
  workflow <- pecanapi::insert_new_workflow(con, site_id, model_id,
                                            start_date = start_date,
                                            end_date = end_date,
                                            notes = notes)

  settings <- list() %>%
    pecanapi::add_workflow(workflow) %>%
    pecanapi::add_database() %>%
    pecanapi::add_pft_list(pft_list) %>%
    pecanapi::add_rabbitmq(model_queue = "ED2_develop") %>%
    modifyList(list(
      meta.analysis = list(iter = 3000, random.effects = FALSE),
      run = list(inputs = list(
        met = list(source = "CRUNCEP", output = "ED2", method = "ncss")
      )),
      ensemble = list(size = ensemble_size, variable = "NPP")
    )) %>%
    modifyList(list(
      run = list(inputs = list(
        lu = list(id = 294),
        soil = list(id = 297),
        thsum = list(id = 295),
        veg = list(id = 296)
      )),
      model = list(
        phenol.scheme = 0,
        edin = "ED2IN.rgit",
        prerun = "ulimit -s unlimited",
        barebones_ed2in = "true",
        ed2in_tags = list(
          IMOUTPUT = 3,  # Monthly analysis files
          ISOUTPUT = 3,  # History files...
          FRQSTATE = 1,  # ...every 1
          UNITSTATE = 2, # ...month
          IOOUTPUT = 0,
          PLANT_HYDRO_SCHEME = 0,
          ISTOMATA_SCHEME = 0,
          ISTRUCT_GROWTH_SCHEME = 0,
          TRAIT_PLASTICITY_SCHEME = as.integer(trait_plasticity),
          ICANRAD = ifelse(multiple_scatter, 1, 2),
          CROWN_MOD = as.integer(crown_model),
          N_PLANT_LIM = as.integer(n_limit_ps),
          N_DECOMP_LIM = as.integer(n_limit_soil),
          INCLUDE_THESE_PFT = "6,9,10,11"
        )
      ),
      workflow = list(nowait = isTRUE(nowait))
    )) %>%
    modifyList(list(...))

  pecanapi::submit_workflow(settings)

  list(
    workflow_id = workflow[["id"]],
    settings = settings
  )
}
