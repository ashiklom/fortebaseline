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
  existing_data <- read_fst(outfile)
  anti_df <- existing_data %>% ...
  # Build the paths for comparison with `all_files`
  # anti_join to create read_files
} else {
  read_files <- all_files
  existing_data <- NULL
}

o_data_list <- future_map(read_files[["o_file"]], read_i_cohort, .progress = TRUE)
o_data_df <- bind_rows(o_data_list)
write_fst(o_data_df, outfile)

rd <- function(f, pb) {
  pb$tick()
  read_i_cohort(f)
}
n <- 100
pb <- progress::progress_bar$new(total = n)
profvis::profvis(
o_data_list <- map(read_files[["o_file"]][seq_len(n)], rd, pb = pb)
)

result <- fast_read_hdf5(ff)

ff <- read_files[["o_file"]]
files <- ff
rxp <- paste0(fs::path_dir(ff)[[1]], "/analysis-I-*.h5") 
## ttest <- system2("h5dump", c("-d FMEAN_RAD_PROFILE_CO", ff), stdout = TRUE)
ttest <- system2("h5dump", c("-d FMEAN_RAD_PROFILE_CO", rxp))
