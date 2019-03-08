# Establish database connection
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

users <- dplyr::tbl(con, "users") %>%
  dplyr::collect()

users %>%
  dplyr::filter(name == "carya") %>%
  dplyr::pull(id)

last_workflow_id <- function(con) {
  import::here("%>%", .from = "magrittr")
  con %>%
    dplyr::tbl("workflows") %>%
    dplyr::filter(id == max(id, na.rm = TRUE)) %>%
    dplyr::pull(id)
}

get_run_ids <- function(con, workflow_id) {
  import::here("%>%", .from = "magrittr")
  ensembles <- con %>%
    dplyr::tbl("ensembles") %>%
    dplyr::select(ensemble_id = id, workflow_id)
  runs <- con %>%
    dplyr::tbl("runs") %>%
    dplyr::select(run_id = id, ensemble_id)
  con %>%
    dplyr::tbl("workflows") %>%
    dplyr::filter(id == workflow_id) %>%
    dplyr::inner_join(ensembles, by = c("id" = "workflow_id")) %>%
    dplyr::inner_join(runs, by = "ensemble_id") %>%
    dplyr::pull(run_id)
}

read_ed_year_lai <- function(year, workflow_id, run_id = NULL,
                             glue_string = "analysis-T-{year}-00-00-000000-g01.h5") { #nolint
  url <- pecanapi::run_dap(workflow_id, glue::glue(glue_string), run_id = run_id) #nolint
  nc <- ncdf4::nc_open(url)
  on.exit(ncdf4::nc_close(nc))
  lai_full <- ncdf4::ncvar_get(nc, "LAI_PY")
  lai_sums <- t(apply(lai_full, c(1, 3), sum))
  colnames(lai_sums) <- as.character(seq_len(ncol(lai_sums)))
  start_date <- lubridate::as_datetime(glue::glue("{year}-01-01 00:00:00"))
  tibble::as_tibble(lai_sums) %>%
    dplyr::mutate(time = start_date +
                    lubridate::minutes(dplyr::row_number() * 30)) %>%
    tidyr::gather("pft", "lai", -time) %>%
    dplyr::mutate(pft = as.integer(pft))
}

# Source test-pecan follow workflow functions
source("~/Projects/pecan_project/test-pecan/follow_workflow.R")
