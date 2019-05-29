library(fortebaseline)
library(tidyverse)
library(fs)

analysis_dir <- path(here::here(), "analysis")
data_dir <- path(analysis_dir, "data")

workflows <- path(data_dir, "derived-data", "current-workflows.csv") %>%
  read_csv(col_types = c("ddlll")) %>%
  mutate(workflow_path = path(data_dir, "model_output", "workflows",
                              paste0("PEcAn_", workflow_id)))

runs <- workflows %>%
  mutate(
    run_id = map(workflow_path, path, "run", "runs.txt") %>%
      map(readLines) %>%
      map(as.numeric)
  ) %>%
  unnest(run_id) %>%
  mutate(
    run_input_path = path(workflow_path, "run", run_id),
    run_output_path = path(workflow_path, "out", run_id)
  )

read_if_exists <- function(x) {
  if (fs::file_exists(x)) {
    readLines(x)
  } else {
    NULL
  }
}

get_last_date <- function(x) {
  rxp <- "Simulating: + ([[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4})"
  sim_lines <- grep(rxp, x)
  last_line <- tail(sim_lines, 1)
  out <- strptime(gsub(rxp, "\\1", last_line), "%m/%d/%Y", tz = "UTC")
  as.POSIXct(out)
}

runs2 <- runs %>%
  mutate(
    logfile = map(path(run_output_path, "logfile.txt"), read_if_exists),
    ## started = map_lgl(logfile, is.null),
    ## last_date = map_if(logfile, started, get_last_date)
  )


runs2 %>%
  filter(map_lgl(last_date, negate(is.null))) %>%
  select(short_id, last_date)

x <- c(lubridate::ymd("2010-01-01"), NA)
class(x)

runs2 %>%
  filter(map_lgl(logfile, negate(is.null))) %>%
  transmute(workflow_id, run_id, map_chr(logfile, tail, n = 1))
