library(fortebaseline)
library(data.table)
library(magrittr)
library(ggplot2)

osoil <- fst_soil()
msoil <- fst_soil_monthly()

soil_start <- with(osoil, osoil[datetime < "1910-01-01", ])
