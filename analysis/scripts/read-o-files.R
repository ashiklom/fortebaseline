#!/usr/bin/env Rscript
library(fortebaseline)
library(tidyverse)
library(fs)
library(furrr)
library(here)
library(fst)

stopifnot(
  requireNamespace("future", quietly = TRUE)
)

future::plan("multiprocess")

workflows_dir <- file.path("/public", "shared-docker-volumes",
                          "pecan_data", "workflows")
if (!file.exists(workflows_dir)) {
  message("Running locally. Using downloaded workflows.")
  workflows_dir <- file.path("analysis", "data", "model_output", "workflows")
}

message("Loading file list...")
all_files <- current_workflows %>%
  mutate(workflow_dir = path(workflows_dir, paste0("PEcAn_", workflow_id))) %>%
  filter(fs::dir_exists(workflow_dir)) %>%
  mutate(
    o_file = workflow_dir %>%
      future_map(fs::dir_ls, regexp = "analysis-I", recurse = TRUE) %>%
      map(as.character)
  ) %>%
  unnest(o_file)
message("Done!")

outfile <- here("analysis", "data", "retrieved", "cohort_output.fst")
if (file.exists(outfile)) {
  # Read file
  existing_data <- read_fst(outfile) %>%
    as_tibble()
  # Build the paths for comparison with `all_files`
  anti_df <- existing_data %>%
    transmute(
      o_file = file.path(workflows_dir,
                         paste0("PEcAn_", format(workflow_id, scientific = FALSE)),
                         "out",
                         format(run_id, scientific = FALSE),
                         strftime(datetime, "analysis-I-%Y-%m-%d-%H%M%S-g01.h5", tz = "UTC"))
    )
  # anti_join to create read_files
  read_files <- anti_join(all_files, anti_df, by = "o_file")
} else {
  message("Output file not found. Reading all files.")
  read_files <- all_files
  existing_data <- NULL
}

message(nrow(read_files), " total remaining files.")

if (nrow(read_files) > 0) {
  message("Reading ", nrow(read_files), " new files.")
  o_data_list <- future_map(read_files[["o_file"]],
                            possibly(read_i_cohort, NULL),
                            .progress = TRUE)
  o_data_df <- bind_rows(existing_data, o_data_list)
  write_fst(o_data_df, outfile)
} else {
  message("No new files to read.")
}
