library(fortebaseline)
library(tidyverse)
stopifnot(requireNamespace("fst", quietly = TRUE))

f <- "analysis/data/derived-data/monthly-ensemble-output.fst"

result <- fst::read_fst(f) %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything()) %>%
  left_join(workflow_structures(), by = "workflow_id")

# LAI by PFT
result %>%
  group_by(workflow_id, crown, rtm, traits, date, pft) %>%
  summarize(lai_mean = mean(lai_co),
            lai_sd = sd(lai_co),
            lai_lo = quantile(lai_co, 0.25),
            lai_hi = quantile(lai_co, 0.75)) %>%
  ggplot() +
  aes(x = date, y = lai_mean, ymin = lai_lo, ymax = lai_hi) +
  geom_ribbon(aes(color = pft, fill = pft), alpha = 0.5) +
  ## geom_line(aes(color = pft)) +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)

# LAI by PFT
result %>%
  filter(date < "1905-01-01") %>%
  group_by(workflow_id, crown, rtm, traits, date, pft) %>%
  summarize(lai_mean = mean(lai_co),
            lai_sd = sd(lai_co),
            lai_lo = quantile(lai_co, 0.25),
            lai_hi = quantile(lai_co, 0.75)) %>%
  ggplot() +
  aes(x = date, y = lai_mean, ymin = lai_lo, ymax = lai_hi) +
  ## geom_ribbon(aes(color = pft, fill = pft), alpha = 0.5) +
  geom_line(aes(color = pft)) +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)


dat <- fst::read_fst(f) %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything())

dat %>%
  filter(date == min(date), runs == min(runs)) %>%
  ## select(starts_with("mmean")) %>%
  select_if(~n_distinct(.x) > 1) %>%
  glimpse()

monthly_means <- dat %>%
  group_by(workflow_id, runs, date) %>%
  select(starts_with("mmean")) %>%
  mutate(
    total_lai = sum(mmean_lai_py),
    leaf_n = sum(mmean_bleaf_n_py), leaf_c = sum(mmean_bleaf_py),
    root_n = sum(mmean_broot_n_py), root_c = sum(mmean_broot_py),
    storage_n = sum(mmean_bstorage_n_py), storage_c = sum(mmean_bstorage_py)
  ) %>%
  select(-mmean_lai_py, -dplyr::matches("bleaf|broot|bstorage")) %>%
  summarize_all(mean) %>%
  rename_all(~gsub("^mmean_(.*)_py$", "\\1", .)) %>%
  left_join(workflow_structures(), by = "workflow_id")

plot_vars <- c("npp", "gpp", "rh", "total_lai")

# These give weird results (LAI > 20...?)
monthly_means %>%
  filter(total_lai > 20) %>%
  select(workflow_id, runs, date) %>%
  distinct(workflow_id, runs) %>%
  print(n = Inf)
  ## semi_join(dat, .)

monthly_means %>%
  filter(crown == "finite", rtm == "two-stream") %>%
  filter(date < "1906-01-01") %>%
  ggplot() +
  aes(x = date, y = carbon_st, color = traits, group = runs) +
  geom_line()

plt <- monthly_means %>%
  select(workflow_id, runs, date,
         crown, rtm, traits,
         !!plot_vars) %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  filter(month %in% 6:8) %>%
  anti_join(filter(monthly_means, total_lai > 20), by = "runs") %>%
  group_by(workflow_id, runs, crown, rtm, traits, year) %>%
  summarize_at(plot_vars, mean) %>%
  gather(variable, value, !!plot_vars) %>%
  ggplot() +
  aes(x = factor(year), y = value,
      fill = interaction(crown, rtm, traits),
      color = interaction(crown, rtm, traits)) +
  ## geom_violin() +
  geom_boxplot() +
  ## geom_line() +
  ## geom_jitter() +
  facet_wrap(vars(variable), scales = "free_y") +
  labs(x = "Year") +
  theme_bw() +
  theme(legend.position = "bottom")
## dv <- imguR::imguR("png", width = 10, height = 8, units = "in", res = 300)
## plt
## imguR::imgur_off(dv)
