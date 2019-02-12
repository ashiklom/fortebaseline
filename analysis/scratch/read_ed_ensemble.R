library(magrittr)

workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
runs <- list.files(outdir)

run <- runs[[1]]

t_files <- list.files(file.path(outdir, run), "analysis-T")
y_files <- list.files(file.path(outdir, run), "analysis-Y")
years <- as.numeric(gsub(".*-T-([[:digit:]]{4})-.*", "\\1", t_files))

tfiles_full <- file.path(outdir, run, t_files)
yfiles_full <- file.path(outdir, run, y_files)

# Read first file
tfile <- t_files[[1]]
yfile <- y_files[[1]]

tfile_full <- file.path(outdir, run, tfile)
yfile_full <- file.path(outdir, run, yfile)
stopifnot(file.exists(tfile_full), file.exists(yfile_full))

nc_y <- ncdf4::nc_open(yfile_full)
nc_t <- ncdf4::nc_open(tfile_full)

all_dates <- seq(
  ISOdatetime(head(years, 1), 1, 1, 0, 0, 0, "UTC"),
  ISOdatetime(tail(years, 1), 12, 31, 23, 59, 59, "UTC"),
  by = as.difftime(30, units = "mins")
)
val_dates <- difftime(all_dates, "1900-01-01 00:00:00UTC",
                      units = "hours")

# Define dimensions
out_dims <- list(
  # phony_dim_0
  phony_dim_0 = ncdf4::ncdim_def(
    "time",
    "hours since 1900-01-01 00:00:00 UTC",
    as.numeric(val_dates),
    unlim = TRUE
  ),
  phony_dim_2 = ncdf4::ncdim_def(
    "soil_depth",
    "m",
    ncdf4::ncvar_get(nc_y, "SLZ")
  ),
  phony_dim_3 = ncdf4::ncdim_def(
    "cohort",
    "",
    seq_len(11)
  ),
  phony_dim_4 = ncdf4::ncdim_def(
    "pft",
    "",
    seq_len(17)
  )
)
basetime <- lubridate::ymd_hms("1900-01-01 00:00:00 UTC")

define_variable <- function(varname) {
  raw_var <- nc_t[[c("var", varname)]]
  var_dimnames <- purrr::map_chr(raw_var[["dim"]], "name") %>%
    intersect(names(out_dims))
  ncdf4::ncvar_def(varname, "", out_dims[var_dimnames])
}

all_vars <- purrr::map(names(nc_t[["var"]]), define_variable)
names(all_vars) <- names(nc_t[["var"]])

ncdf4::nc_close(nc_y)
ncdf4::nc_close(nc_t)

outfile <- file.path(outdir, run, "all_analysis.nc")
out_nc <- ncdf4::nc_create(outfile, all_vars)

# Loop over years:
for (i in seq_along(years)) {
  # Open year's -T- file
  year <- years[[i]]
  year_start <- ISOdatetime(year, 1, 1, 0, 0, 0, "UTC")
  ## toffset <- as.integer(i + difftime(year_start, basetime, units = "hour") *
  ##                         (i - 1))
  nc_t <- ncdf4::nc_open(tfiles_full[[i]])

  #   Loop over variables:
  for (v in names(all_vars)) {
    values <- ncdf4::ncvar_get(nc_t, v)
    vcount <- dim(values)
    if (is.null(vcount)) vcount <- 1
    ndim <- length(vcount)
    vstart <- rep(1, ndim)
    vstart[[ndim]] <- toffset
    ncdf4::ncvar_put(out_nc, v, values, start = vstart, count = vcount)
  }
}

# Dimensions of T files
# 0 - time (30 min)
# 1 - site (1)
# 2 - soil layers (9)
# 3 - cohort
# 4 - PFT

# Create the NetCDF file
# - Get list of variables and their dimensions
#   - Define the 5 dimensions
#   - Loop through first 

##################################################
library(magrittr)
library(data.table)
library(ggplot2)

workflow_id <- 99000000066
outdir <- file.path("analysis", "data", "model_output", workflow_id)
runs <- list.files(outdir)

run <- runs[[1]]
rundir <- file.path(outdir, run)

read_variable <- function(nc, varname) {
  var_dimnames <- purrr::map_chr(nc[[c("var", varname, "dim")]], "name")
  value <- ncdf4::ncvar_get(nc, varname, collapse_degen = FALSE)
  value_long <- data.table::melt(value)
  data.table::setDT(value_long)
  data.table::setnames(value_long, c(var_dimnames, "value"))
  value_long
}

read_year <- function(rundir, year, varnames) {
  filename <- file.path(
    rundir,
    glue::glue("analysis-T-{year}-00-00-000000-g01.h5")
  )
  nc <- ncdf4::nc_open(filename)
  raw_data <- data.table::rbindlist(
    purrr::map(varnames, read_variable, nc = nc),
    use.names = TRUE,
    fill = TRUE,
    idcol = "id"
  )
  raw_data[, variable := varnames[id]][, id := NULL]
  raw_data[, dtime := ISOdatetime(year, 1, 1, 0, 0, 0, "UTC") +
               lubridate::dhours(phony_dim_0 / 2)]
  suppressWarnings({
    raw_data[, phony_dim_0 := NULL]
    raw_data[, phony_dim_1 := NULL]
    raw_data[, phony_dim_1 := NULL]
  })
  data.table::setnames(
    raw_data,
    c("phony_dim_2", "phony_dim_3", "phony_dim_4"),
    c("soil", "cohort", "PFT"),
    skip_absent = TRUE
  )
  raw_data
}

nc <- ncdf4::nc_open(list.files(rundir, "-T-.*h5", full.names = TRUE)[[1]])
all_varnames <- names(nc[["var"]])
ncdf4::nc_close(nc)

varnames <- c(
  ## "SLOW_SOIL_C_PY",
  ## "FMEAN_CARBON_AC_PY",
  ## "FMEAN_CARBON_ST_PY",
  "FMEAN_NPP_PY",
  "FMEAN_LAI_PY",
  "FMEAN_SOIL_WATER_PY",
  "FMEAN_PSI_OPEN_PY",
  "FMEAN_PSI_CLOSED_PY"
)
all_data <- purrr::map(seq(1902, 1990), read_year,
                       rundir = rundir, varnames = varnames) %>%
  data.table::rbindlist(use.names = TRUE)
monthly <- all_data[, mean(value),
                    by = list(variable,
                              ddate = lubridate::year(dtime))]
ggplot(monthly) +
  aes(x = ddate, y = V1) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y")
