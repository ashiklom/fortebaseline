#!/usr/bin/env Rscript

stopifnot(
  requireNamespace("stringr", quietly = TRUE),
  requireNamespace("bibtex", quietly = TRUE)
)

arg <- commandArgs(trailingOnly = TRUE)
infile <- arg[1]
outfile <- arg[2]
reffile <- arg[3]

if (is.na(infile)) infile <- "analysis/paper/paper.Rmd"
if (is.na(outfile)) outfile <- "analysis/paper/references.bib"
if (is.na(reffile)) reffile <- "~/Dropbox/references/library.bib"

stopifnot(
  file.exists(infile),
  file.exists(reffile)
)

message("Searching for citations in file ", infile)
raw_file <- readLines(infile)
rxp <- "@[[:alnum:]_]+"
results_raw <- stringr::str_match_all(raw_file, rxp)
citations <- sort(unique(do.call(c, results_raw)))
citations <- stringr::str_remove(citations, "@")

# Read bibtex file
message("Reading bibfile...")
bibfile <- bibtex::read.bib(reffile)
selected_references <- bibfile[citations]
message("Writing ", length(selected_references), " citations to file ", outfile)
bibtex::write.bib(selected_references, outfile)
message("Done!")
