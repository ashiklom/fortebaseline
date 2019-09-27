library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

out_root <- getOption("fortebaseline.ed_root")
ed_src <- getOption("fortebaseline.ed_src_dir")
ed_alt <- getOption("fortebaseline.ed_alt")

stopifnot(dir_exists(out_root), dir_exists(ed_src), file_exists(ed_alt))

# Load parameters
trait_distribution <- readRDS(here("analysis", "data",
                                   "retrieved", "trait-distribution.rds"))

medians <- trait_distribution %>%
  select(name = bety_name, trait, Median) %>%
  pivot_wider(id_cols = "name", names_from = "trait",
              values_from = "Median") %>%
  as_pft_list()

## means <- trait_distribution %>%
##   select(name = bety_name, trait, Mean) %>%
##   pivot_wider(id_cols = "name", names_from = "trait",
##               values_from = "Mean") %>%
##   as_pft_list()

runs <- crossing(
  ## nesting(datatype = c("mean", "median"),
  ##         trait_values = list(means, medians)),
  nesting(datatype = "median",
          trait_values = list(medians)),
  trait_plasticity = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  crown_model = c(TRUE, FALSE)
) %>%
  mutate(
    casename = paste0(
      datatype, "-",
      if_else(crown_model, "F", "C"),
      if_else(multiple_scatter, "M", "T"),
      if_else(trait_plasticity, "P", "S")
    ),
    start_date = "1902-01-01",
    end_date = "1999-12-31",
    ed_exe = ed_alt
  ) %>%
  select(-datatype) %>%
  select(casename, everything())

rawout <- pmap(runs, run_ed_maybe)

# Check status
if (interactive()) map_chr(rawout, ~.x$log(n = 1))

map(rawout, ~.x$wait())
results <- rawout %>%
  map("outdir") %>%
  map_dfr(read_efile_dir)

results %>%
  rename(casename = basename) %>%
  right_join(runs, "casename") %>%
  select(casename, trait_plasticity, multiple_scatter, crown_model,
         scalar, cohort, soil, pft_py = pft, trait_values, outdir) %>%
  saveRDS(here("analysis", "data", "retrieved", "structure-median.rds"))
