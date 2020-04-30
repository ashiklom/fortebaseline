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

## Performing ED2 simulations (optional)

We provide processed versions of our complete ED-2.2 output via the companion [OSF repository](https://osf.io/dznuf/), and our analysis code (next section) is set up to download these results automatically.
You can therefore skip this section unless you want to re-do our ED2 ensemble simulations.

Code for performing the ED-2.2 ensemble simulations is performed in several steps.
First, draw the parameters for the parameter ensembles.
This can be done using the `analysis/scripts/draw-params.R` script.
The `trait-distribution.rds` file that this script depends on can be downloaded from OSF (see link above).
This script will create a CSV file of trait samples, which is used as input to the subsequent step.

Next, generate the ED input files (`ED2IN` namelist and `config.xml` parameter file) based on the input parameter distributions.
This is performed by the `analysis/scripts/create-ed-cases.R` script, which will populate a directory with a large (~4000) number of subdirectories---one for each ED2 simulation---containing the configuration files (and eventually, outputs).
Note that the configuration files use absolute paths for input files; default prefix paths in this script are configured for our project on the PNNL HPC, so you will need to adjust these for your own use.

Once the cases have been created and uploaded, each case can be run with a command like the following:

```sh
/path/to/ed2/executable -f /path/to/<case>/ED2IN
```

For additional help on performing these simulations (including help on getting ED2 compiled and running on your machine), refer to the ED2 documentation, open an issue in this repository, or contact Alexey Shiklomanov directly.

## Performing ED2 default and posterior median simulations (optional)

Similarly to the above, this step is not strictly necessary because we provide the outputs via OSF.
However, if you would like to reproduce these runs yourself, you can do so with two scripts:
`analysis/scripts/structure-default.R` will do simulations under default ED2 parameters for all model structures, and
`analysis/scripts/structure-mean-median.R` will do the same for posterior median ED2 parameters.

## Analyzing the ED2 results

Once the ED2 runs have been completed, they can be analyzed via the provided [`drake`](https://github.com/ropensci/drake)-powered workflow.
To execute this workflow, run the `analysis/drake.R` script (e.g. `Rscript analysis/drake.R` from the shell).
This should automatically download our results from OSF and perform all of the intermediate analysis steps necessary to generate the figures in our paper.
