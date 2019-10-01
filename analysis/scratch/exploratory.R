# Setup ----
library(fortebaseline)
library(fs)
library(here)
library(tidyverse)
library(lubridate, exclude = "here")

out_root <- getOption("fortebaseline.ed_root")
ed_src <- getOption("fortebaseline.ed_src_dir")
ed_alt <- getOption("fortebaseline.ed_alt")
figdir <- dir_create("analysis", "figures", "org-mode")

stopifnot(dir_exists(out_root), dir_exists(ed_src), file_exists(ed_alt))

trait_distribution <- readRDS(here("analysis", "data",
                                   "retrieved", "trait-distribution.rds"))

median_df <- trait_distribution %>%
  select(name = bety_name, trait, Median) %>%
  pivot_wider(id_cols = "name", names_from = "trait",
              values_from = "Median")

# Helper functions ---
annual_mean <- function(dat) {
  dat %>%
    mutate(year = lubridate::year(datetime)) %>%
    select(-datetime) %>%
    group_by_at(vars(-value)) %>%
    summarize_all(mean) %>%
    ungroup()
}
long_pft <- function(dat) {
  dat %>%
    select(casename = basename, pft) %>%
    unnest(pft) %>%
    select(-c(case:param_id)) %>%
    mutate(pft = set_pft(pft)) %>%
    pivot_longer(-c(casename:datetime, pft)) %>%
    filter(lubridate::month(datetime) %in% 6:8) %>%
    annual_mean()
}
lai_compare <- function(dat, scales = "fixed", yvar = "mmean_lai_py") {
  bind_rows(default_long, dat) %>%
    mutate(casename = fct_inorder(casename)) %>%
    filter(name == !!yvar) %>%
    ggplot() +
    aes(x = year, y = value, color = casename, group = casename) +
    geom_line() +
    facet_grid(vars(pft), scales = scales)
}

# Parameter plot ---
edparams <- ed_default_params() %>%
  semi_join(trait_distribution, "trait")
trait_distribution %>%
  mutate(pft = factor(pft, pfts("pft"))) %>%
  ## tidyr::unnest(draws) %>%
  ggplot() +
  aes(x = pft, fill = pft) +
  ## geom_violin(aes(y = draws)) +
  geom_point(aes(y = Median), size = 3, col = "blue1") +
  geom_point(aes(y = default_value), data = edparams,
             size = 3, col = "red1") +
  facet_wrap(vars(trait), scales = "free") +
  scale_fill_manual(values = pfts("color")) +
  labs(x = "PFT", fill = "PFT") +
  cowplot::theme_cowplot() +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = c(0.6, 0.1))

# Default results ---
default_out <- read_efile_dir(path(out_root, "default-CTS"))
default_long <- long_pft(default_out) %>%
  filter(year <= 1920)

# Median results ---
median_run <- median_df %>%
  as_pft_list() %>%
  run_ed_maybe(
    "median-CTS-new",
    trait_values = .,
    end_date = "1920-01-01"
  )
median_run$log(n = 1)
median_out <- read_efile_dir(path(out_root, "median-CTS"))
median_long <- long_pft(median_out) %>%
  filter(year <= 1920)
lai_compare(median_long)

# Growth respiration ---
growth_resp <- median_df %>%
  select(name, growth_resp_factor) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-growth-resp",
    trait_values = .,
    end_date = "1920-01-01"
  )
gr_out <- read_efile_dir(growth_resp$outdir)
gr_long <- long_pft(gr_out)
gr_long %>% lai_compare("free_y")

# Vcmax ---
vcmax <- median_df %>%
  select(name, Vcmax) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-vcmax",
    trait_values = .,
    end_date = "1920-01-01"
  )
vcmax_out <- read_efile_dir(vcmax$outdir)
vcmax_long <- long_pft(vcmax_out)
vcmax_long %>% lai_compare("free_y")

# Fine root to leaf ratio --- 
fr2l <- median_df %>%
  select(name, fineroot2leaf) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-fr2l",
    trait_values = .,
    end_date = "1920-01-01"
  )
fr2l_out <- read_efile_dir(fr2l$outdir)
fr2l_long <- long_pft(fr2l_out)
fr2l_long %>% lai_compare("free_y")

# Quantum efficiency ---
quanteff <- median_df %>%
  select(name, quantum_efficiency) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-quanteff",
    trait_values = .,
    end_date = "1920-01-01"
  )
quanteff_out <- read_efile_dir(quanteff$outdir)
quanteff_long <- long_pft(quanteff_out)
quanteff_long %>% lai_compare("free_y")

# Specific leaf area ---
sla <- median_df %>%
  select(name, SLA) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-sla",
    trait_values = .,
    end_date = "1920-01-01"
  )
sla_out <- read_efile_dir(sla$outdir)
sla_long <- long_pft(sla_out)
sla_long %>% lai_compare()

# Specific leaf area + growth respiration ---
sla_gr <- median_df %>%
  select(name, SLA, growth_resp_factor) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-sla-gr",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(sla_gr$p$wait(), silent = FALSE)
sla_gr_out <- read_efile_dir(sla_gr$outdir)
sla_gr_long <- long_pft(sla_gr_out)
sla_gr_long %>% lai_compare()

# SLA + growth resp. + Vcmax ---
sla_gr_vm <- median_df %>%
  select(name, SLA, growth_resp_factor, Vcmax) %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-sla-gr-vm",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(sla_gr_vm$p$wait(), silent = TRUE)
sla_gr_vm_out <- read_efile_dir(sla_gr_vm$outdir)
sla_gr_vm_long <- long_pft(sla_gr_vm_out)
sla_gr_vm_long %>% lai_compare()

# SLA + growth resp. + Vcmax (low Pine) ---
sla_gr_vm2_dat <- median_df %>%
  select(name, SLA, growth_resp_factor, Vcmax) %>%
  mutate(Vcmax = if_else(name == "umbs.northern_pine", 50, Vcmax))

sla_gr_vm2 <- sla_gr_vm2_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-sla-gr-vm2",
    trait_values = .,
    end_date = "1920-01-01"
  )

while (sla_gr_vm2$p$is_alive()) {
  writeLines(sla_gr_vm2$log(n = 3))
  Sys.sleep(3)
}

sla_gr_vm2_out <- read_efile_dir(sla_gr_vm2$outdir)
sla_gr_vm2_long <- long_pft(sla_gr_vm2_out)
sla_gr_vm2_long %>% lai_compare()

# SLA + Vcmax ---
sla_vm_dat <- median_df %>%
  select(name, SLA, Vcmax)
  ## mutate(Vcmax = if_else(name == "umbs.northern_pine", 50.0, Vcmax))

sla_vm <- sla_vm_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "vary-sla-vm",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(sla_vm$p$wait(), silent = TRUE)
sla_vm_out <- read_efile_dir(sla_vm$outdir)
sla_vm_long <- long_pft(sla_vm_out)
sla_vm_long %>% lai_compare()

# Median parameters, but low growth resp. for hardwoods ---
trait_distribution %>%
  filter(trait == "growth_resp_factor") %>%
  select(pft, Median, lo, hi)
low_gr_dat <- median_df %>%
  mutate(growth_resp_factor = if_else(name == "umbs.northern_pine",
                                      growth_resp_factor, 0.1))
low_gr <- low_gr_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "median-low-gr",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(low_gr$p$wait(), silent = TRUE)
low_gr_out <- read_efile_dir(low_gr$outdir)
low_gr_long <- long_pft(low_gr_out)
low_gr_long %>% lai_compare()

# Median parameters, but low growth resp. for hardwoods and ED2 default SLA ---
low_gr_nosla_dat <- median_df %>%
  select(-SLA) %>%
  mutate(growth_resp_factor = if_else(name == "umbs.northern_pine",
                                      growth_resp_factor, 0.1))
low_gr_nosla <- low_gr_nosla_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "median-low-gr-no-sla",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(low_gr_nosla$p$wait(), silent = TRUE)
low_gr_nosla_out <- read_efile_dir(low_gr_nosla$outdir)
low_gr_nosla_long <- long_pft(low_gr_nosla_out)
low_gr_nosla_long %>% lai_compare()

# Median parameters, but ED2 default SLA ---
edparams %>%
  filter(trait == "SLA") %>%
  select(name = bety_name, trait, default_value) %>%
  pivot_wider(names_from = "trait", values_from = "default_value",
              names_prefix = "def_") %>%
  left_join(median_df %>% select(name, SLA), "name")

nosla_dat <- median_df %>%
  select(-SLA)
nosla <- nosla_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "median-no-sla",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(nosla$p$wait(), silent = TRUE)
nosla_out <- read_efile_dir(nosla$outdir)
nosla_long <- long_pft(nosla_out)
nosla_long %>% lai_compare()

# Is the problem Pine SLA specifically?
# Let's run with median parameters for everything except pine SLA, which is at its default value
median_df %>%
  select(name, SLA)
median_nopinesla_dat <- median_df %>%
  mutate(SLA = if_else(grepl("pine", name), 2.88, SLA))
median_nopinesla <- median_nopinesla_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "median_nopinesla",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(median_nopinesla$p$wait(), silent = TRUE)
read_efile_dir(median_nopinesla$outdir) %>% long_pft() %>% lai_compare()

# Across all Pinus species in TRY, the mean seems to be around 4.5. What happens if we do that?
median_df %>%
  select(name, SLA)
median_trypinesla_dat <- median_df %>%
  mutate(SLA = if_else(grepl("pine", name), 4.5, SLA))
median_trypinesla <- median_trypinesla_dat %>%
  as_pft_list() %>%
  run_ed_maybe(
    "median_trypinesla",
    trait_values = .,
    end_date = "1920-01-01"
  )
try(median_trypinesla$p$wait(), silent = TRUE)
read_efile_dir(median_trypinesla$outdir) %>% long_pft() %>% lai_compare()

trait_distribution %>%
  filter(trait == "SLA") %>%
  select(shortname, Mean, Median, lo, hi)
