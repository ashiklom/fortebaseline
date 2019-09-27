library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

future::plan(future.callr::callr())

out_root <- getOption("fortebaseline.ed_root")
ed_src <- getOption("fortebaseline.ed_src_dir")
ed_alt <- getOption("fortebaseline.ed_alt")

stopifnot(dir_exists(out_root), dir_exists(ed_src), file_exists(ed_alt))

structure_runs <- expand_grid(
  trait_plasticity = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  crown_model = c(TRUE, FALSE)
) %>%
  mutate(casename = paste0("default-",
                           if_else(crown_model, "F", "C"),
                           if_else(multiple_scatter, "M", "T"),
                           if_else(trait_plasticity, "P", "S")),
         start_date = "1902-01-01",
         end_date = "1999-12-31",
         ed_exe = ed_alt)

structure_raw <- pmap(structure_runs, run_ed_maybe)

outdir <- path(out_root, "default-CTS")

structure_results <- structure_raw %>%
  map("outdir") %>%
  map_dfr(read_efile_dir)

structure_results %>%
  rename(casename = basename) %>%
  right_join(structure_runs, "casename") %>%
  select(casename, trait_plasticity, multiple_scatter, crown_model,
         scalar, cohort, soil, pft_py = pft, outdir) %>%
  saveRDS(here("analysis", "data", "retrieved", "structure-default.rds"))
