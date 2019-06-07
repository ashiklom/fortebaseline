library(fortebaseline)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(rlang, include.only = "syms")

con <- default_connection()

start_date <- "1902-06-01"
end_date <- "2000-12-31"

# Get status of current workflows
bety_workflows <- tbl(con, "workflows") %>%
  filter(start_date == !!start_date,
         end_date == end_date)

run_matrix <- tibble(
  crown_model = c(TRUE, FALSE),
  ## n_limit_ps = c(TRUE, FALSE),
  ## n_limit_soil = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  trait_plasticity = c(TRUE, FALSE)
) %>%
  expand(., !!!(syms(colnames(.))))

if (!isTRUE(start_workflows)) {
  message("Performing a test run instead.")
  test_run <- run_ed_ensemble(start_date = start_date,
                              end_date = "1902-08-31",
                              nowait = FALSE,
                              ensemble_size = 2)
  stop("Set `start_workflows` to `TRUE` interactively to actually run.")
}
runs <- purrr::pmap(run_matrix, run_ed_ensemble,
                    start_date = start_date,
                    end_date = end_date,
                    nowait = TRUE,
                    ensemble_size = 20)

workflows <- run_matrix %>%
  mutate(workflow_id = map(runs, "workflow_id") %>% reduce(c),
         short_id = as.numeric(workflow_id - 99000000000)) %>%
  select(workflow_id, short_id, everything())

write_csv(
  workflows,
  file.path("analysis", "data", "derived-data", "current-workflows.csv")
)

if (FALSE) {

  bety_workflows <- tbl(con, "workflows") %>%
    rename(workflow_id = id) %>%
    filter(workflow_id %in% workflows[["workflow_id"]])

  workflows %>%
    slice(20) %>%
    pull(workflow_id) %>%
    pecanapi::run_url("logfile.txt") %>%
    tail_file(start_at = Inf)
  ## writeLines()
}

if (FALSE) {
  con2 <- DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    user = "bety",
    password = "bety",
    host = "localhost",
    port = 7990
  )
  out <- PEcAn.DB::db.query("SELECT * FROM workflows WHERE id = 99000000054", con2)
  out[["started_at"]]
}
