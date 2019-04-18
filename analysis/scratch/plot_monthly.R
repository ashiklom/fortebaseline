library(tidyverse)
stopifnot(requireNamespace("fst", quietly = TRUE))

f <- "analysis/data/derived-data/monthly-ensemble-output.fst"
dat <- fst::read_fst(f) %>% as_tibble()

dat %>%
  filter(date == min(date), runs == min(runs)) %>%
  select(starts_with("mmean")) %>%
  select_if(~n_distinct(.x) > 1) %>%
  glimpse()

monthly_means <- dat %>%
  group_by(workflows, runs, date) %>%
  select(starts_with("mmean")) %>%
  mutate(
    total_lai = sum(mmean_lai_py),
    leaf_n = sum(mmean_bleaf_n_py), leaf_c = sum(mmean_bleaf_py),
    root_n = sum(mmean_broot_n_py), root_c = sum(mmean_broot_py),
    storage_n = sum(mmean_bstorage_n_py), storage_c = sum(mmean_bstorage_py)
  ) %>%
  select(-mmean_lai_py, -matches("bleaf|broot|bstorage")) %>%
  summarize_all(mean) %>%
  rename_all(~gsub("^mmean_(.*)_py$", "\\1", .))

plot_vars <- c("npp", "gpp", "rh", "total_lai")

# These give weird results (LAI > 20...?)
monthly_means %>%
  filter(total_lai > 20) %>%
  select(workflows, runs, date) %>%
  distinct(workflows, runs) %>%
  print(n = Inf)
  ## semi_join(dat, .)

monthly_means %>%
  select(workflows, runs, date, !!plot_vars) %>%
  filter(lubridate::month(date) == 7) %>%
  gather(variable, value, -workflows, -runs, -date) %>%
  ggplot() +
  aes(x = factor(date), y = value, fill = workflows) +
  geom_boxplot() +
  facet_wrap(vars(variable), scales = "free_y")
