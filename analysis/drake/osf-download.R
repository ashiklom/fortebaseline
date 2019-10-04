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

