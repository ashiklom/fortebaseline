library(pecanapi)
library(tidyverse)

workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

years <- seq(1902, 1990)

run_ids <- readLines(output_url(workflow_id, "run/runs.txt"))

for (runid in run_ids) {
  message("Run ID: ", runid)
  rundir <- file.path(outdir, runid)
  dir.create(rundir, recursive = TRUE, showWarnings = FALSE)

  pb <- progress::progress_bar$new(total = length(years))
  for (year in years) {
    pb$tick()
    ncfile <- paste0(year, ".nc")

    target_file <- file.path(rundir, ncfile)
    if (!file.exists(target_file)) {
      tryCatch(
        download.file(
          run_url(workflow_id, ncfile, runid),
          target_file,
          quiet = TRUE
        ),
        error = function(e) {
          message("Failed to download. Skipping to next file.")
        }
      )
    }

    analysis_file <- paste0("analysis-T-", year, "-00-00-000000-g01.h5")
    analysis_full_file <- file.path(rundir, analysis_file)
    if (!file.exists(analysis_full_file)) {
      tryCatch(
        download.file(
          run_url(workflow_id, analysis_file, runid),
          analysis_full_file,
          quiet = TRUE
        ),
        error = function(e) {
          message("Failed to download. Skipping to next file.")
        }
      )
    }

    y_file <- paste0("analysis-Y-", year, "-00-00-000000-g01.h5")
    y_full_file <- file.path(rundir, y_file)
    if (!file.exists(y_full_file)) {
      tryCatch(
        download.file(
          run_url(workflow_id, y_file, runid),
          y_full_file,
          quiet = TRUE
        ),
        error = function(e) {
          message("Failed to download. Skipping to next file.")
        }
      )
    }
  }
}
