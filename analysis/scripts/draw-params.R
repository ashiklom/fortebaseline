library(fortebaseline)
library(tidyverse)

set.seed(12345678)
nparams <- 500

stopifnot(
  requireNamespace("drake", quietly = TRUE),
  requireNamespace("here", quietly = TRUE)
)

ret_dir <- here::here("analysis", "data", "retrieved")
params_raw <- readRDS(file.path(ret_dir, "trait-distribution.rds"))

params <- params_raw %>%
  select(-num, -bety_name) %>%
  left_join(params_raw %>%
              distinct(pft, num, bety_name) %>%
              filter(!is.na(num)), "pft") %>%
  unnest(draws) %>%
  filter(trait != "cuticular_cond") %>%
  select(name = bety_name, trait, draws)

param_draws <- params %>%
  group_by(name, trait) %>%
  sample_n(nparams) %>%
  mutate(param_id = row_number()) %>%
  spread(trait, draws) %>%
  ungroup() %>%
  select(param_id, everything())

if (interactive()) {
  param_draws %>%
    gather(trait, value, -param_id, -name) %>%
    filter(!is.na(value)) %>%
    mutate(name = fct_inorder(name)) %>%
    ggplot() +
    aes(x = name, y = value) +
    geom_violin(aes(fill = name)) +
    facet_wrap(vars(trait), scales = "free_y")
}

write_csv(param_draws, file.path(ret_dir, "input-parameters.csv"))
