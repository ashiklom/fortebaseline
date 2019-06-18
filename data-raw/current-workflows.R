library(here, include.only = "here")
library(readr)
library(usethis)
library(dplyr)

current_workflows <- here("data-raw", "current-workflows.csv") %>%
  read_csv(col_types = c("ddlll"))

use_data(current_workflows, overwrite = TRUE)
