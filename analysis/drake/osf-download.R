get_timestamp <- function(osf_id) {
  stopifnot(requireNamespace("osfr", quietly = TRUE))
  osfr::osf_retrieve_file(osf_id) %>%
    dplyr::pull(meta) %>%
    purrr::pluck(1, "attributes", "date_modified")
}

osf_url <- function(osf_id) file.path("https://osf.io/download/", osf_id)

### Parameter uncertainty input files
ensemble_params_file <- path(download_dir, "input-parameters.csv")
params_osf <- "87ku4"

trait_distribution_file <- path(download_dir, "trait-distribution.rds")
td_osf <- "bfyuh"

plan <- bind_plans(plan, drake_plan(
  run_params_dl = target(
    download.file(osf_url(params_osf), file_out(!!ensemble_params_file)),
    trigger = trigger(change = get_timestamp(params_osf))
  ),
  trait_distribution_dl = target(
    download.file(osf_url(td_osf), file_out(!!trait_distribution_file)),
    trigger = trigger(change = get_timestamp(td_osf))
  )
))

### Parameter uncertainty output files
stop()
cohort_file <- path(download_dir, "all-output-monthly-cohort.fst")
cohort_osf <- "..."
pft_file <- path(download_dir, "all-output-monthly-pft.fst")
pft_osf <- "..."
scalar_file <- path(download_dir, "all-output-monthly-scalar.fst")
scalar_osf <- "..."
soil_file <- path(download_dir, "all-output-monthly-soil.fst")
soil_osf <- "..."

plan <- bind_plans(plan, drake_plan(
  cohort_file_dl = target(
    download.file(osf_url(cohort_osf), file_out(!!cohort_file)),
    trigger = trigger(change = get_timestamp(cohort_osf),
                      condition = !file.exists(cohort_file))
  ),
))

