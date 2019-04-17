library(tidyverse)
stopifnot(
  requireNamespace("future"),
  requireNamespace("furrr"),
  requireNamespace("here")
)

monthly_mean_vars <- readLines(
  here::here("analysis", "data",
             "derived-data", "monthly_mean_vars.txt")
)

monthly_pft_vars <- readLines(
  here::here("analysis", "data",
             "derived-data", "monthly_pft_vars.txt")
)

monthly_cohort_vars <- readLines(
  here::here("analysis", "data",
             "derived-data", "monthly_cohort_vars.txt")
)

monthly_cb_vars <- readLines(
  here::here("analysis", "data",
             "derived-data", "monthly_cb_vars.txt")
)


setpft <- function(i) {
  stopifnot(all(i %in% c(6, 9, 10, 11)))
  pfts <- c(
    rep(NA_character_, 5),
    "Pine", NA_character_, NA_character_,
    "Early hardwood", "Mid hardwood", "Late hardwood"
  )
  ipfts <- pfts[i]
  factor(ipfts, pfts[c(9:11, 6)])
}

read_month <- function(file, pkg = c("RNetCDF", "ncdf4")) {
  pkg <- match.arg(pkg)
  stopifnot(requireNamespace(pkg, quietly = TRUE))

  if (pkg == "ncdf4") {
    nc <- ncdf4::nc_open(file)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    getnc <- purrr::partial(ncdf4::ncvar_get, nc = nc)
    allnc <- -1
  } else if (pkg == "RNetCDF") {
    nc <- RNetCDF::open.nc(file)
    on.exit(RNetCDF::close.nc(nc), add = TRUE)
    getnc <- purrr::partial(RNetCDF::var.get.nc, ncfile = nc)
    allnc <- NA
  }

  file_base <- basename(file)
  month <- gsub("^.*-E-[[:digit:]]{4}-([[:digit:]]{2})-.*$", "\\1", file_base) %>%
    as.numeric()

  # Extract PFT data
  pft_i <- getnc("PFT")
  pft <- setpft(pft_i)
  pft_data <- purrr::map(monthly_pft_vars, getnc) %>%
    # Sum across DBH classes
    purrr::map(rowSums) %>%
    purrr::map_dfc(`[`, pft_i) %>%
    setNames(monthly_pft_vars) %>%
    dplyr::mutate(
      pft = pft,
      REPRO_PA = getnc("REPRO_PA")[pft_i]
    ) %>%
    dplyr::select(pft, dplyr::everything()) %>%
    dplyr::rename_all(tolower)

  means <- purrr::map(monthly_mean_vars, getnc) %>%
    setNames(monthly_mean_vars)

  # Carbon budget data for the current month
  cbdata <- purrr::map_dfc(monthly_cb_vars, getnc,
                           start = c(month, 1), count = c(1, allnc)) %>%
    setNames(monthly_cb_vars)

  out <- purrr::map_dfc(monthly_cohort_vars, getnc) %>%
    setNames(monthly_cohort_vars) %>%
    dplyr::mutate(pft = pft, !!!means, !!!cbdata) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(pft, hite, lai_co, dplyr::everything()) %>%
    dplyr::left_join(pft_data, by = "pft")
}

result_dir <- file.path("analysis", "data", "model_output", "workflows")

future::plan("multiprocess")
result <- tibble(
  workflows = list.files(result_dir),
  workflow_paths = file.path(result_dir, workflows),
  runs = purrr::map(workflow_paths, ~list.files(file.path(.x, "out")))
) %>%
  unnest() %>%
  mutate(
    run_paths = file.path(workflow_paths, "out", runs),
    month_files = purrr::map(run_paths, list.files, pattern = "analysis-E")
  ) %>%
  unnest() %>%
  mutate(
    month_path = file.path(run_paths, month_files),
    date = lubridate::ymd(
      paste0(str_extract(month_files, "[[:digit:]]{4}-[[:digit:]]{2}"), "-01")
    ),
    data = furrr::future_map(month_path, read_month, .progress = TRUE)
  )

outdir <- here::here("analysis/data/derived-data")
dir.create(outdir, showWarnings = FALSE)
result %>%
  unnest() %>%
  fst::write_fst(file.path(outdir, "monthly-ensemble-output.fst"))
