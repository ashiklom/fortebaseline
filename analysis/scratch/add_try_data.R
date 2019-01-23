library(data.table)

tryfile <- "~/Projects/try-raw-data/4143.txt"

trydata <- fread(tryfile)

# Subset to selected species 
species <- readLines(file.path("analysis", "data", "derived-data", "species_list.txt"))

setkey(trydata, "AccSpeciesName")
species_data <- trydata[species, ]

nrow(species_data)
