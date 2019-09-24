library(fs)

met_dir <- "~/Projects/forte_project/umbs-ed-inputs/met/CUSTOM_ED2_site_1-33/"
nc <- ncdf4::nc_open(path(met_dir, "1902JUN.h5"))

par(mfrow = c(4, 4))
for (v in names(nc$var)) {
  plot(ncdf4::ncvar_get(nc, v), type = "l", main = v)
}

dir_ls(path("analysis", "data", "retrieved"))
