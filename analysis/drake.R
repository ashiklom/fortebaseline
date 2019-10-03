#!/usr/bin/env Rscript
if (getRversion() >= "3.6") {
  options(conflicts.policy = "strict")
  conflictRules("testthat", exclude = "matches")
  conflictRules("drake", exclude = c("gather", "expand", "plan"))
  conflictRules("dplyr",
                mask.ok = c("filter", "lag", "intersect",
                            "setdiff", "setequal", "union"))
  conflictRules("lubridate",
                mask.ok = c("as.difftime", "date"),
                exclude = c("intersect", "setdiff", "union", "here",
                            "stamp"))
  conflictRules("data.table", exclude = c("between", "first", "last",
                                          "transpose", "hour", "isoweek",
                                          "mday", "minute", "month",
                                          "quarter", "second", "wday", "week",
                                          "yday", "year"))
} else {
  warning("Package conflict resolution requires R >= 3.6. ",
          "This script may not work as expected.",
          immediate. = TRUE)
}

suppressPackageStartupMessages({
  library(drake)
  # Data processing
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(purrr)
  library(lubridate)
  library(readr)
  # Parallel execution
  library(future)
  library(future.callr)
  library(furrr)
  # Files and file paths
  library(fs)
  library(fst)
  library(here)
  # Plotting
  library(ggplot2)
  library(ggrepel)
  library(cowplot)
  library(fortebaseline)
})

cmdargs <- commandArgs(trailingOnly = TRUE)

# Set common directories and paths
analysis_dir <- here("analysis")
stopifnot(dir_exists(analysis_dir))
data_dir <- dir_create(path(analysis_dir, "data"))
download_dir <- dir_create(path(data_dir, "retrieved"))
fig_dir <- dir_create(path(analysis_dir, "figures"))
drake_dir <- path(analysis_dir, "drake")
stopifnot(dir_exists(drake_dir))

plan <- drake_plan()
src_files <- dir_ls(drake_dir, regexp = "zzskip", invert = TRUE)
# Possibly here to allow skipping files with `stop()`
walk(src_files, possibly(source, NULL))

if ("--poster" %in% cmdargs) source("analysis/drake_poster.R")
if ("--postgrad" %in% cmdargs) source("analysis/drake_postgrad.R")
if ("--paper" %in% cmdargs) {
  plan <- bind_plans(plan, drake_plan(
  param_table = file_in(!!(path(data_dir, "derived-data",
                                "parameter-table.csv"))) %>%
    read_csv(),
    paper = target(
      rmarkdown::render(
        knitr_in(!!(path(analysis_dir, "paper", "paper.Rmd"))),
        .format
      ),
      transform = map(.format = c("html_document", "pdf_document")))
    ))
}

# Parallelism configuration. Not sure which of these is better...
future::plan(future.callr::callr)

dconf <- drake_config(
  plan,
  parallelism = "future",
  jobs = availableCores()
)

if ("make" %in% cmdargs) {
  message("Running `drake::make`")
  make(config = dconf)
} else if (!interactive()) {
  dconf
}
