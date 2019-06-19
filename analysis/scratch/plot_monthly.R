## library(fortebaseline)
library(tidyverse)
library(lubridate)
devtools::load_all(here::here(), attach_testthat = FALSE)
stopifnot(requireNamespace("fst", quietly = TRUE))

raw_monthly_output %>%
  select(dplyr::matches("agb"))

raw_monthly_output %>%
  filter(date == tail(date, 1), runs == unique(runs)[[3]]) %>%
  select(-(workflow_id:date))

  select(pft, nplant, nplant_py, mmean_lai_py, lai_co) %>%
  mutate(lai = lai_co * nplant,
         lai_sum = cumsum(lai))

f <- "analysis/data/derived-data/monthly-ensemble-output.fst"

raw_monthly_output %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything()) %>%
  left_join(workflow_structures(), by = "workflow_id")

result <- fst::read_fst(f) %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything()) %>%
  left_join(workflow_structures(), by = "workflow_id")

monthly_subset %>%
  mutate(
    year = floor_date(date, "years") %m+% lubridate::period(6, "months"),
    month = month(date)
  ) %>%
  filter(month %in% 7:9) %>%
  gather(variable, value, GPP:AGB) %>%
  mutate(variable = fct_inorder(variable)) %>%
  group_by(variable, year, add = TRUE) %>%
  summarize(
    mean = mean(value),
    lo = quantile(value, 0.2),
    hi = quantile(value, 0.8)
  ) %>%
  filter(!is.na(variable)) %>%
  mutate(model = interaction(crown, rtm, traits)) %>%
  ggplot() +
  aes(x = year, y = mean, ymin = lo, ymax = hi, fill = model, color = model) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_cowplot() +
  theme(axis.title.y = element_blank())
  

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

monthly_means <- raw_monthly_output %>%
  group_by(workflow_id, crown, rtm, traits, date, runs, pft) %>%
  select(starts_with("mmean"), agb_py) %>%
  summarize_all(mean)

monthly_means %>%
  select(
    GPP = mmean_gpp_py,
    NPP = mmean_npp_py,
    LAI_pft = mmean_lai_py,
    AGB_pft = agb_py
  ) %>%
  summarize(
    GPP = mean(GPP),
    NPP = mean(NPP),
    LAI = sum(LAI_pft),
    AGB = sum(AGB_pft)
  )

monthly_means %>%
  filter(date > "1910-07-01", runs == unique(runs)[[1]]) %>%
  select(pft, dplyr::matches("lai"))


  summarize(
    total_lai = unique(mmean_lai_py),
    leaf_n = unique(mmean_bleaf_n_py), leaf_c = unique(mmean_bleaf_py),
    root_n = unique(mmean_broot_n_py), root_c = unique(mmean_broot_py),
    storage_n = unique(mmean_bstorage_n_py), storage_c = unique(mmean_bstorage_py)
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

#########################################
library(fortebaseline)
library(tidyverse)
library(here)
library(fst)
library(lubridate, exclude = "here")

cohort_file <- here("analysis", "data", "retrieved", "cohort_output.fst") %>%
  fst()

use_cols <- c("workflow_id", "run_id", "datetime", "pft", "nplant",
              "bleaf", "bsapwooda", "bstorage",
              "mean_gpp_co", "fmean_npp_co", "lai_co")

raw_data <- as_tibble(cohort_file[, use_cols]) %>%
  semi_join(current_workflows, by = "workflow_id")

instant <- raw_data %>%
  group_by(workflow_id, run_id, datetime) %>%
  summarize(
    agb = sum((bleaf + bsapwooda + bstorage) * nplant),
    gpp = sum(fmean_gpp_co), npp = sum(fmean_npp_co),
    lai = sum(lai_co)
  ) %>%
  ungroup()

annual_jja <- instant %>%
  filter(lubridate::month(datetime) %in% 6:8) %>%
  group_by(workflow_id, run_id, year = year(datetime)) %>%
  summarize_at(vars(-datetime), mean) %>%
  ungroup()

annual_jja_long <- annual_jja %>%
  inner_join(current_workflows, by = "workflow_id") %>%
  gather(variable, value, agb:lai) %>%
  mutate(variable = factor(variable, c("gpp", "npp", "agb", "lai")))

annual_jja_smry <- annual_jja_long %>%
  group_by(model_split, color, variable, year) %>%
  summarize(lo = quantile(value, 0.1),
            hi = quantile(value, 0.9))

my_labeller <- labeller(
  variable = as_labeller(c(
    "gpp" = "GPP ~ (kgC ~ year^{-1})",
    "npp" = "NPP ~ (kgC ~ year^{-1})",
    "agb" = "AGB ~ (kgC)",
    "lai" = "LAI"
  ), default = label_parsed),
  .default = label_value
)

ggplot(annual_jja_long) +
  aes(x = year, y = value, group = run_id) +
  geom_line(color = "grey50", alpha = 0.5) +
  geom_ribbon(aes(ymin = lo, ymax = hi, y = NULL, group = NULL,
                  fill = color),
              data = annual_jja_smry, alpha = 0.7) +
  scale_fill_identity() +
  facet_grid(vars(variable), vars(model_split), scales = "free_y",
             switch = "y", labeller = my_labeller) +
  cowplot::theme_cowplot() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.placement = "outside",
        strip.background = element_blank())
