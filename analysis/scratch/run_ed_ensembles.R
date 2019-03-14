library(fortebaseline)
devtools::load_all(".")

# begin imports
import::from("dplyr", "tbl", "filter", "mutate", "everything", "select", "slice", "pull", .into = "")
import::from("tibble", "tibble", "as_tibble", .into = "")
import::from("tidyr", "expand", .into = "")
import::from("rlang", "syms", .into = "")
import::from("purrr", "map", "reduce", .into = "")
# end imports

con <- default_connection()

start_date <- "1902-06-01"
end_date <- "1912-12-31"

run_matrix <- tibble(
  crown_model = c(TRUE, FALSE),
  n_limit_ps = c(TRUE, FALSE),
  n_limit_soil = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  trait_plasticity = c(TRUE, FALSE)
) %>%
  expand(., !!!(syms(colnames(.))))

runs <- purrr::pmap(run_matrix, run_ed_ensemble,
                    start_date = start_date,
                    end_date = end_date)

workflows <- run_matrix %>%
  mutate(workflow_id = map(runs, "workflow_id") %>% reduce(c)) %>%
  select(workflow_id, everything())

if (FALSE) {
  workflows %>%
    slice(9) %>%
    pull(workflow_id) %>%
    pecanapi::run_url("logfile.txt") %>%
    readLines() %>%
    tail_file(start_at = Inf)
  ## writeLines()
}
