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

message("Loading file list...")
all_files <- read_csv(here("analysis", "data", "derived-data", "current-workflows.csv")) %>%
  mutate(
    workflow_dir = path("analysis", "data", "model_output", "workflows",
                        paste0("PEcAn_", workflow_id)),
    o_file = workflow_dir %>%
      future_map(fs::dir_ls, regexp = "analysis-I", recurse = TRUE) %>%
      map(as.character)
  ) %>%
  unnest(o_file)
message("Done!")

outfile <- here("analysis", "data", "model_output", "cohort_output.fst")
if (file.exists(outfile)) {
  # Read file
  existing_data <- read_fst(outfile) %>%
    as_tibble()
  # Build the paths for comparison with `all_files`
  anti_df <- existing_data %>%
    transmute(
      o_file = file.path("analysis", "data", "model_output", "workflows",
                         paste0("PEcAn_", format(workflow_id, scientific = FALSE)),
                         "out",
                         format(run_id, scientific = FALSE),
                         strftime(datetime, "analysis-I-%Y-%m-%d-%H%M%S-g01.h5", tz = "UTC"))
    )
  # anti_join to create read_files
  read_files <- anti_join(all_files, anti_df, by = "o_file")
} else {
  read_files <- all_files
  existing_data <- NULL
}

if (nrow(read_files) > 0) {
  o_data_list <- future_map(read_files[["o_file"]], read_i_cohort, .progress = TRUE)
  save(o_data_list, file = "o_data_list.RData")
  o_data_df <- bind_rows(existing_data, o_data_list)
  write_fst(o_data_df, outfile)
  file.remove("o_data_list.RData")
} else {
  message("No new files to read.")
}
