#!/usr/bin/env Rscript
library(tidyverse)

wf_ids <- current_workflows %>%
  pull(workflow_id) %>%
  paste(collapse = ",") %>%
  paste0("{", ., "}")

wf <- system2("ssh", c("pecan", "find",
                       paste0("/public/shared-docker-volumes/pecan_data/workflows/PEcAn_", wf_ids),
                       "-name", "analysis-I-*"),
              stdout = TRUE)

wf_data <- wf %>%
  str_match("PEcAn_([[:digit:]]+)/out/([[:digit:]]+)/analysis-I-([[:digit:]-]+)") %>%
  `colnames<-`(c("raw", "workflow", "run", "date")) %>%
  as_tibble() %>%
  mutate(
    workflow = as.numeric(workflow),
    run = as.numeric(run),
    date = lubridate::ymd_hms(date)
  )

wf_data %>%
  group_by(workflow, run) %>%
  filter(date == max(date)) %>%
  select(-raw) %>%
  print(n = Inf)
