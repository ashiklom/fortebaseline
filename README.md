---
output: github_document
---

# fortebaseline

This repository contains the source code and raw manuscript files for the manuscript Shiklomanov et al. (2020) "Structure and parameter uncertainty in centennial projections of forest community structure and carbon cycling". _Global Change Biology_
Step-by-step instructions for reproducing the analysis are below.
(Note that all code assumes you are running R from the repository root directory).

## Setup

First, note that this repository uses `renv` to manage R package dependencies.
Launching R from inside this repository should lead `renv` to configure itself automatically.
If that doesn't work, you can do this manually with:

``` r
# If `renv` isn't installed:
install.packages("renv")
renv::restore()
```

...and then run `renv::restore()` to download and install the right versions of all R packages.

Much of the code for reproducing this analysis is organized as an R package.
You can install this package by running (from the root directory of this repository):

``` r
devtools::install(".")
```

## Performing ED2 simulations

Code for performing the ED-2.2 ensemble simulations is performed in two steps.
The first step generates the ED input files (`ED2IN` namelist and `config.xml` parameter file) based on the input parameter distributions.
This is performed by the `analysis/scripts/create-ed-cases.R` script.

## Installation

You can install the development version of fortebaseline from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ashiklom/fortebaseline")
```
