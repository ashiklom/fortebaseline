library(tidyverse)
library(fortebaseline)

requireNamespace("fst", quietly = TRUE)

runs <- workflow_run_matrix(type = "run")
configs <- runs %>%
  mutate(config = map(path, possibly(read_configxml, NULL))) %>%
  filter(map_lgl(config, negate(is.null))) %>%
  unnest()

fst::write_fst(configs, "analysis/data/derived-data/ed-ensemble-params.fst")

