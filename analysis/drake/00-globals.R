# File names and OSF URLs. These have to be loaded first so that they are
# evaluated during drake plan construction.

ensemble_params_file <- path(download_dir, "input-parameters.csv")
params_osf <- "87ku4"

trait_distribution_file <- path(download_dir, "trait-distribution.rds")
td_osf <- "bfyuh"

cohort_file <- path(download_dir, "all-output-monthly-cohort.fst")
cohort_osf <- "cem5g"

pft_file <- path(download_dir, "all-output-monthly-pft.fst")
pft_osf <- "4z27j"

scalar_file <- path(download_dir, "all-output-monthly-scalar.fst")
scalar_osf <- "tpgm4"

soil_file <- path(download_dir, "all-output-monthly-soil.fst")
soil_osf <- "vg4wr"

default_results_file <- here::here("analysis", "data",
                                     "retrieved", "structure-default.rds")
default_results_osf <- "q9cpf"

median_results_file <- here("analysis", "data", "retrieved",
                            "structure-median.rds")
median_results_osf <- "e5kjr"
