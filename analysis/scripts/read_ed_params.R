library(tidyverse)
library(fortebaseline)

stopifnot(requireNamespace("here", quietly = TRUE))

read_params <- function(workflow_id,
                        workflow_dir = "/public/shared-docker-volumes/pecan_data/workflows") {
  workflow_path <- file.path(workflow_dir,
                             paste0("PEcAn_", format(workflow_id, scientific = FALSE)))
  stopifnot(file.exists(workflow_path))
  ensemble_file <- file.path(workflow_path, "samples.Rdata")
  stopifnot(file.exists(ensemble_file))
  slist <- load_local(ensemble_file)
  runfile <- file.path(workflow_path, "run", "runs.txt")
  stopifnot(file.exists(runfile))
  runs <- as.numeric(readLines(runfile))
  ens_df <- slist[["ensemble.samples"]] %>%
    purrr::map(dplyr::as_tibble) %>%
    purrr::map(~dplyr::mutate(.x, run_id = runs)) %>%
    dplyr::bind_rows(.id = "bety_name") %>%
    dplyr::left_join(
      dplyr::select(pfts(), bety_name, pft),
      by = "bety_name"
    ) %>%
    dplyr::mutate(workflow_id = workflow_id) %>%
    dplyr::select(workflow_id, run_id, pft, dplyr::everything()) %>%
    dplyr::select(-bety_name)
}

params <- current_workflows %>%
  pull(workflow_id) %>%
  map_dfr(read_params)

outdir <- here::here("analysis", "data", "retrieved")
dir.create(outdir, showWarnings = FALSE)
outfile <- file.path(outdir, "ensemble-params.csv")
write_csv(params, outfile)

token <- getOption("osf.token")
if (requireNamespace("osfr", quietly = TRUE) && !is.null(token)) {
  message("Uploading file to OSF")
  osfr::osf_auth(token = token)
  osfr::osf_retrieve_node("dznuf") %>%
    osfr::osf_ls_files() %>%
    dplyr::filter(name == "processed_model_outputs") %>%
    osfr::osf_upload(outfile, name = "ensemble-params.csv", overwrite = TRUE)
}
