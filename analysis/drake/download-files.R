# Download files if they don't exist
if (file.exists(cohort_file)) {
  plan <- bind_plans(plan, drake_plan(
    cohort_file_dl = !!cohort_file
  ))
} else {
  plan <- bind_plans(plan, drake_plan(
    cohort_file_dl = target(
      download.file(osf_url(cohort_osf), file_out(!!cohort_file)),
      trigger = trigger(change = get_timestamp(cohort_osf),
                        condition = !file.exists(cohort_file)),
      hpc = FALSE
    )
  ))
}

if (file.exists(pft_file)) {
  plan <- bind_plans(plan, drake_plan(
    pft_file_dl = !!pft_file
  ))
} else {
  plan <- bind_plans(plan, drake_plan(
    pft_file_dl = target(
      download.file(osf_url(pft_osf), file_out(!!pft_file)),
      trigger = trigger(change = get_timestamp(pft_osf),
                        condition = !file.exists(pft_file)),
      hpc = FALSE
    )
  ))
}

if (file.exists(scalar_file)) {
  plan <- bind_plans(plan, drake_plan(
    scalar_file_dl = !!scalar_file
  ))
} else {
  plan <- bind_plans(plan, drake_plan(
    scalar_file_dl = target(
      download.file(osf_url(scalar_osf), file_out(!!scalar_file)),
      trigger = trigger(change = get_timestamp(scalar_osf),
                        condition = !file.exists(scalar_file)),
      hpc = FALSE
    )
  ))
}

if (file.exists(soil_file)) {
  plan <- bind_plans(plan, drake_plan(
    soil_file_dl = !!soil_file
  ))
} else {
  plan <- bind_plans(plan, drake_plan(
    soil_file_dl = target(
      download.file(osf_url(soil_osf), file_out(!!soil_file)),
      trigger = trigger(change = get_timestamp(soil_osf),
                        condition = !file.exists(soil_file)),
      hpc = FALSE
    )
  ))
}

## # Original code
## plan <- bind_plans(plan, drake_plan(
##   cohort_file_dl = target(
##     download.file(osf_url(cohort_osf), file_out(!!cohort_file)),
##     trigger = trigger(change = get_timestamp(cohort_osf),
##                       condition = !file.exists(cohort_file)),
##     hpc = FALSE
##   ),
##   pft_file_dl = target(
##     download.file(osf_url(pft_osf), file_out(!!pft_file)),
##     trigger = trigger(change = get_timestamp(pft_osf),
##                       condition = !file.exists(pft_file)),
##     hpc = FALSE
##   ),
##   scalar_file_dl = target(
##     download.file(osf_url(scalar_osf), file_out(!!scalar_file)),
##     trigger = trigger(change = get_timestamp(scalar_osf),
##                       condition = !file.exists(scalar_file)),
##     hpc = FALSE
##   ),
##   soil_file_dl = target(
##     download.file(osf_url(soil_osf), file_out(!!soil_file)),
##     trigger = trigger(change = get_timestamp(soil_osf),
##                       condition = !file.exists(soil_file)),
##     hpc = FALSE
##   )
## ))
