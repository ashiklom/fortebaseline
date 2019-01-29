library(data.table)
import::from(magrittr, "%>%")

tryfile <- "~/Projects/try-raw-data/4143.txt"

trydata <- fread(tryfile)

# Subset to selected species 
species <- readLines(file.path("analysis", "data",
                               "derived-data", "species_list.txt"))

setkey(trydata, "AccSpeciesName")

species_data <- trydata[
  species,
  .(AccSpeciesName, ObservationID,
    TraitID, TraitName,
    DataID, DataName,
    ValueKindName, StdValue, UnitName,
    Reference)
][!is.na(StdValue)][ValueKindName == "Single"]

con <- dbConnect(
  Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

bety2try <- tibble::tribble(
  ~bety_name, ~DataID,
  "SLA", 6582,
  "c2n_leaf", 455,
  "c2n_fineroot", 489,
  "Vcmax", 550,
  "fineroot2leaf", 5383,
  "leaf_width", 447,
  "root_respiration_rate", 1189
) %>%
  dplyr::inner_join(
    dplyr::tbl(con, "variables") %>% dplyr::select(id, name),
    by = c("bety_name" = "name"),
    copy = TRUE
  )

setDT(bety2try)

species_data_sub <- species_data[bety2try, on = "DataID"]

bety_species <- con %>%
  dplyr::tbl("species") %>%
  dplyr::filter(scientificname %in% !!species) %>%
  dplyr::select(AccSpeciesName = scientificname,
                specie_id = id) %>%
  dplyr::collect()

setDT(bety_species)
species_data_final <- bety_species[species_data_sub, on = "AccSpeciesName"]

stmt <- DBI::dbSendStatement(con, paste(
  "INSERT INTO traits (variable_id, specie_id, mean, n, notes)",
  "VALUES ($1, $2, $3, 1, '__TRY-DB__')"
))
qry <- DBI::dbBind(stmt, list(
  species_data_final[["id"]],
  species_data_final[["specie_id"]],
  species_data_final[["StdValue"]]
))
DBI::dbClearResult(qry)

## con %>%
##   dplyr::tbl("traits") %>%
##   dplyr::filter(notes %like% "__TRY-DB__") %>%
##   dplyr::count(specie_id)

# The rest of these are not available in TRY, at least for the
# species we're using.
## "stomatal_slope"
## "f_labile", ,
## "seedling_mortality",
## "nonlocal_dispersal",
## "growth_resp_factor",
## "mort2",
## "Vm_low_temp",
## "leaf_turnover_rate",
## "root_turnover_rate",
## "quantum_efficiency",
## "water_conductance",
## "r_fract",

# Code for searching TRY data names 
## species_data[grepl("stomat", DataName, ignore.case = TRUE),
##              .N,
##              by = .(DataID, DataName)][order(N, decreasing = TRUE)]
