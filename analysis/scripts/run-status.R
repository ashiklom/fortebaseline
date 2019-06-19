#!/usr/bin/env Rscript
library(fortebaseline)
library(tidyverse)
library(lubridate, exclude = "here")
library(here)

wf_string <- sprintf("{%s}", paste(current_workflows$workflow_id, collapse = ","))

wf <- system2("ssh", c("pecan", "find",
                       paste0("/public/shared-docker-volumes/pecan_data/workflows/PEcAn_",
                              wf_string),
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
  ungroup() %>%
  group_by(workflow, date_lt = ceiling_date(date, "25 years")) %>%
  count()
