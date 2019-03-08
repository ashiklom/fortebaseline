read_t_file_var <- function(outdir, run_id) {
  requireNamespace("stringr", quietly = TRUE)
  import::from("magrittr", "%>%", .into = "")
}

if (FALSE) {
  archive_dir <- "~/Unsynced/archived-outputs/"
  workflow_id <- 99000000066
  workflow_dir <- file.path(archive_dir, paste0("PEcAn_", workflow_id))
  outdir <- file.path(workflow_dir, "out")
  runs <- list.files(outdir)
  run_id <- runs[[1]]

  # Part of `read_ensemble_t_files` function
  rundir <- file.path(outdir, run_id)
  t_files <- list.files(rundir, "-T-.*.h5")
  years <- stringr::str_match(t_files, "-T-([[:digit:]]{4})-")[, 2] %>%
    as.numeric()

  filename <- file.path(rundir, t_files[[1]])
  year <- years[[1]]

  variable <- "LAI_PY"
  use_pfts <- c(6, 8, 9, 10, 11)
  time_average <- "month"

  stopifnot(
    time_average %in% c("day", "week", "month",
                        "bimonth", "quarter", "season",
                        "halfyear", "year")
  )

  nc <- ncdf4::nc_open(filename)
  data_raw <- ncdf4::ncvar_get(nc, variable, collapse_degen = FALSE)
  dim_names <- nc[[c("var", variable, "dim")]] %>%
    purrr::map_chr("name")
  dim_dict <- c(
    "phony_dim_0" = "itime",
    "phony_dim_1" = "site",
    "phony_dim_2" = "soil_layer",
    "phony_dim_3" = "cohort",
    "phony_dim_4" = "pft"
  )[dim_names]

  basedate <- ISOdatetime(year, 01, 01, 0, 0, 0, "UTC")
  
  data_long <- data.table::melt(data_raw) %>%
    `colnames<-`(c(dim_dict, variable)) %>%
    tibble::as_tibble() %>%
    dplyr::select(-site) %>%
    dplyr::filter(pft %in% use_pfts) %>%
    dplyr::mutate(
      time = basedate + 0.5 * lubridate::dhours(itime)
    ) %>%
    dplyr::select(-itime) %>%
    dplyr::group_by(pft, cohort, time) %>%
    dplyr::summarize()
}

fix_tdim <- function(raw_dim) {
  stopifnot(raw_dim %in% paste0("phony_dim_", 0:4))
}
