#!/usr/bin/env Rscript
#SBATCH --account=br20_shik544
#SBATCH --time=00-00:15
#SBATCH --nodes 1

# Script to check that running ED2 from R actually works on PIC.
library(fortebaseline)

options(
  fortebaseline.ed_root = fs::dir_create(here::here(
    "analysis", "data", "retrieved", "ed-tests"
  )),
  ## fortebaseline.ed_input_dir = fs::path_home(
  ##   "forte-data", "ed"
  ## ),
  fortebaseline.ed_input_dir = fs::path_home(
    "projects", "pnnl", "forte-gcb", "forte_project",
    "umbs-ed-inputs"
  ),
  ## fortebaseline.ed_src_dir = fs::path_home("ed2")
  fortebaseline.ed_src_dir = fs::path_home(
    "projects", "models", "forte-ed2"
  )
)

p <- run_ed_maybe(
  ## "pic-test",
  "macos-test",
  end_date = "1908-01-01",
  overwrite = TRUE
)
p$p$wait()
result <- read_efile_dir(p$outdir)
result
