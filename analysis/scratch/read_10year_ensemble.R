library(tidyverse)
stopifnot(
  requireNamespace("future"),
  requireNamespace("furrr")
)

read_month <- function(file) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  v <- nc[["var"]]
  vnames <- purrr::map_chr(v, "name")
  vdims <- purrr::map(v, "dim")
  i <- which(lengths(vdims) == 1 &
               purrr::map_chr(vdims, list(1, "name")) == "phony_dim_0")
  purrr::map_dfc(names(i), ncdf4::ncvar_get, nc = nc) %>%
    setNames(names(i)) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(pft, hite, lai_co, dplyr::everything())
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
  ) %>%
  unnest()
