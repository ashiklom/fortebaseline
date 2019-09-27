library(fortebaseline)
library(tidyverse)
library(fs)
library(here)
library(lubridate, exclude = "here")

fix_cases <- function(x) {
  x2 <- gsub("^.*-", "", x)
  mod_levels <- c("CTS", "CTP", "CMS", "CMP",
                  "FTS", "FTP", "FMS", "FMP")
  factor(x2, mod_levels)
}

structure_results <- here("analysis", "data", "retrieved",
                          "structure-default.rds") %>%
  readRDS() %>%
  mutate(
    runtype = "default",
    casename = fix_cases(casename)
  )

structure_results %>%
  select(casename, cohort) %>%
  unnest(cohort) %>%
  filter(
    month(datetime) == 7,
    year(datetime) %in% c(1910, 1920, 1950, 1980)
  ) %>%
  mutate(
    pft = set_pft(pft),
    datetime = year(datetime)
  ) %>%
  ggplot() +
  aes(x = mmean_light_level_co, y = hite) +
  geom_line() +
  geom_point(aes(color = pft)) +
  facet_grid(vars(casename), vars(datetime)) +
  theme_bw() +
  labs(
    x = "Relative light level",
    y = "Cohort height (m)",
    color = "PFT"
  ) +
  scale_color_manual(values = pfts("color")) +
  theme(legend.position = "bottom")

ggsave("analysis/figures/default-light-levels.png",
       width = 7.16, height = 5.96)

# Now compare default and median parameters

median_results <- here("analysis", "data", "retrieved",
                       "structure-median.rds") %>%
  readRDS() %>%
  mutate(
    runtype = "median",
    casename = fix_cases(casename)
  )

both_results <- bind_rows(structure_results, median_results)

annual_mean <- function(dat) {
  dat %>%
    mutate(year = lubridate::year(datetime)) %>%
    select(-datetime) %>%
    group_by_at(vars(-value)) %>%
    summarize_all(mean) %>%
    ungroup()
}

both_long_s <- both_results %>%
  select(casename, runtype, scalar) %>%
  unnest(scalar) %>%
  select(-c(case:param_id), -age, -area, -area_si,
         -latitude, -longitude, -starts_with("mmsq")) %>%
  pivot_longer(-c(casename:datetime)) %>%
  annual_mean()

both_long_s %>%
  filter(grepl("mmean_(gpp|npp|plresp)_py", name)) %>%
  ggplot() +
  aes(x = year, y = value, color = runtype) +
  geom_line() +
  facet_grid(vars(name), vars(casename), scales = "free_y")

both_long_p <- both_results %>%
  select(casename, runtype, pft_py) %>%
  unnest(pft_py) %>%
  select(-c(case:param_id)) %>%
  mutate(pft = set_pft(pft)) %>%
  pivot_longer(-c(casename:datetime, pft)) %>%
  filter(month(datetime) %in% 6:8) %>%
  annual_mean()

both_long_p %>%
  filter(name == "mmean_lai_py") %>%
  ggplot() +
  aes(x = year, y = value, color = runtype, group = runtype) +
  geom_line() +
  facet_grid(vars(pft), vars(casename))

# Look at each model indidivually
dcrown <- structure_results %>%
  filter(!multiple_scatter, !trait_plasticity) %>%
  mutate(crown_model = if_else(crown_model, "finite", "closed") %>%
           factor(c("closed", "finite")))

dcrown_sa <- dcrown %>%
  select(crown_model, scalar) %>%
  unnest(scalar) %>%
  select(-c(case:param_id)) %>%
  pivot_longer(-c(crown_model, datetime)) %>%
  annual_mean()

dcrown_sa %>%
  filter(name %in% c("mmean_gpp_py", "mmean_npp_py")) %>%
  ggplot() +
  aes(x = year, y = value, color = crown_model, group = crown_model) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y")

dcrown_pa <- dcrown %>%
  select(crown_model, pft_py) %>%
  unnest(pft_py) %>%
  select(-c(case:param_id)) %>%
  pivot_longer(-c(crown_model:pft)) %>%
  filter(month(datetime) %in% 6:8) %>%
  annual_mean() %>%
  mutate(pft = set_pft(pft))

dcrown_pa %>%
  filter(name == "mmean_bstorage_py") %>%
  ggplot() +
  aes(x = year, y = value, color = crown_model, group = crown_model) +
  geom_line() +
  facet_grid(vars(pft), scales = "free_y")

dcrown_ca <- dcrown %>%
  select(crown_model, cohort) %>%
  unnest(cohort) %>%
  select(-c(case:param_id))

## dcrown_ca %>%
structure_results %>%
  select(casename, cohort) %>%
  unnest(cohort) %>%
  filter(
    month(datetime) == 7,
    year(datetime) %in% c(1910, 1920, 1950, 1980)
  ) %>%
  mutate(pft = set_pft(pft),
         datetime = year(datetime)) %>%
  ggplot() +
  aes(x = dbh, y = hite, color = pft) +
  geom_segment(aes(xend = 0, yend = hite)) +
  geom_point() +
  facet_grid(vars(datetime), vars(casename))

