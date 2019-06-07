library(tidyverse)
wf <- system2("ssh", c("pecan", "find",
                        "/public/shared-docker-volumes/pecan_data/workflows/PEcAn_99000000{144..151}",
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
