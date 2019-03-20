library(fortebaseline)
devtools::load_all(".")

# begin imports
import::from("dplyr", "tbl", "filter", "mutate", "everything", "select", "slice", "pull", "rename", .into = "")
import::from("tibble", "tibble", "as_tibble", .into = "")
import::from("tidyr", "expand", .into = "")
import::from("rlang", "syms", .into = "")
import::from("purrr", "map", "reduce", .into = "")
# end imports

con <- default_connection()

start_date <- "1902-06-01"
end_date <- "1912-12-31"

# Get status of current workflows
bety_workflows <- tbl(con, "workflows") %>%
  filter(start_date == !!start_date,
         end_date == end_date)


run_matrix <- tibble(
  crown_model = c(TRUE, FALSE),
  n_limit_ps = c(TRUE, FALSE),
  n_limit_soil = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  trait_plasticity = c(TRUE, FALSE)
) %>%
  expand(., !!!(syms(colnames(.))))

stop("This will start the workflows!")
runs <- purrr::pmap(run_matrix, run_ed_ensemble,
                    start_date = start_date,
                    end_date = end_date)

workflows <- run_matrix %>%
  mutate(workflow_id = map(runs, "workflow_id") %>% reduce(c)) %>%
  select(workflow_id, everything())

bety_workflows <- tbl(con, "workflows") %>%
  rename(workflow_id = id) %>%
  filter(workflow_id %in% workflows[["workflow_id"]])

if (FALSE) {
  workflows %>%
    slice(9) %>%
    pull(workflow_id) %>%
    pecanapi::run_url("logfile.txt") %>%
    readLines() %>%
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
