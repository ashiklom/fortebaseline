library(tidyverse)
library(fortebaseline)

requireNamespace("fst", quietly = TRUE)

runs <- workflow_run_matrix(type = "run")

path <- runs$path[[1]]

configs <- runs %>%
  mutate(config = map(path, possibly(read_configxml, NULL))) %>%
  filter(map_lgl(config, negate(is.null))) %>%
  unnest()

fst::write_fst(configs, "analysis/data/derived-data/ed-ensemble-params.fst")

meta_vars <- configs %>%
  select(-workflow_id, -path) %>%
  group_by(pft) %>%
  summarize_all(~length(unique(.x))) %>%
  gather(variable, count, -pft, -run_id) %>%
  filter(count > 1) %>%
  distinct(variable) %>%
  pull()

params <- configs %>%
  select(workflow_id, run_id, pft, !!!meta_vars)

params_long <- params %>%
  gather(param, value, -workflow_id, -run_id, -pft)

ggplot(params_long) +
  aes(x = pft, y = value, color = pft, fill = pft) +
  geom_violin(color = "black", alpha = 0.5) +
  geom_jitter() +
  facet_wrap(vars(param), scales = "free_y")
