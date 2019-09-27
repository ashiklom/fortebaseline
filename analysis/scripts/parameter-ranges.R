library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

out_root <- getOption("fortebaseline.ed_root")
ed_src <- getOption("fortebaseline.ed_src_dir")
ed_alt <- getOption("fortebaseline.ed_alt")

stopifnot(dir_exists(out_root), dir_exists(ed_src), file_exists(ed_alt))

params <- list(
  c2n_leaf = c(10, 15, 20, 25, 30, 40, 50, 75),
  clumping_factor = c(0.25, 0.35, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
  f_labile = c(0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.85, 0.95),
  fineroot2leaf = c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 1.5, 2),
  growth_resp_factor = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.75),
  leaf_reflect_nir = c(0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6),
  leaf_reflect_vis = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.15, 0.2, 0.25),
  leaf_respiration_rate_m2 = c(0.1, 0.2, 0.3, 0.5, 0.75, 1, 1.5, 2),
  leaf_trans_nir = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
  leaf_trans_vis = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.125, 0.15, 0.2),
  # leaf turnover -- pine-only
  mort1 = c(0.5, 1, 2, 3, 5, 10, 20, 40),
  mort2 = c(1, 5, 10, 20, 30, 40, 50, 60),
  mort3 = c(0.0025, 0.005, 0.0075, 0.01, 0.0125, 0.015, 0.0175, 0.02),
  nonlocal_dispersal = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 0.95),
  orient_factor = c(-0.4, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.45),
  quantum_efficiency = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.11, 0.13, 0.15),
  r_fract = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
  repro_min_h = c(2, 4, 6, 8, 10, 15, 20, 30),
  root_respiration_rate = c(0.5, 0.75, 1, 2, 3, 4, 5, 7.5),
  root_turnover_rate = c(1.25, 2.5, 3.75, 5, 6.25, 7.5, 8.75, 10),
  seedling_mortality = c(0.6, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1),
  SLA = c(2.5, 5, 10, 15, 20, 25, 30, 40),
  stomatal_slope = c(2.25, 3, 5, 7, 9, 11, 13, 15),
  Vcmax = c(5, 10, 15, 20, 30, 40, 50, 60),
  water_conductance = c(1e-5, 2e-5, 5e-5, 1e-4, 5e-4, 1e-3, 1e-2, 1e-1)
)

trait_vals <- bind_cols(params) %>%
  gather(key = name, value = value) %>%
  deframe() %>%
  as.list() %>%
  lmap(~list(umbs.eary_hardwood = .x))
