library(data.table, exclude = c("between", "first", "last"))
library(magrittr, include.only = "%>%")
library(RPostgres)
library(fst)
library(dplyr, mask.ok = c("filter", "lag", "intersect",
                           "setdiff", "setequal", "union"))
library(tidyr)
library(stringr)
library(fortebaseline)

con <- dbConnect(
  Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

fst_file <- "~/Projects/try-raw-data/4143.fst"
if (!file.exists(fst_file)) {
  message("Creating new FST file.")
  tryfile <- "~/Projects/try-raw-data/4143.txt"
  trydata <- fread(tryfile)
  fst::write_fst(trydata, fst_file)
} else {
  message("Reading existing FST file")
  trydata <- read_fst(fst_file, as.data.table = TRUE)
}

# Subset to selected species 
species_df <- pfts_species()
species <- species_df %>% pull(scientificname)

setkey(trydata, "AccSpeciesName")

species_data <- trydata[
  species,
  .(AccSpeciesName, ObservationID,
    TraitID, TraitName,
    DataID, DataName,
    ValueKindName, StdValue, UnitName,
    Reference)
][!is.na(StdValue)][ValueKindName == "Single"]

bety2try <- tibble::tribble(
  ~bety_name, ~DataID,
  "SLA", 6582,
  ## "c2n_leaf", 455,
  ## "c2n_fineroot", 489,
  ## "leaf_width", 447,
  "Vcmax", 550,
  "fineroot2leaf", 5383,
  "leaf_respiration_rate_m2", 69,
  "root_respiration_rate", 1189
) %>%
  inner_join(
    tbl(con, "variables") %>% select(id, name),
    by = c("bety_name" = "name"),
    copy = TRUE
  )

setDT(bety2try)

species_data_sub <- species_data[bety2try, on = "DataID"]

if (FALSE) {
  ggplot(species_data_sub) +
    aes(x = AccSpeciesName, y = StdValue) +
    geom_violin() +
    geom_jitter(size = 0.5, alpha = 0.5) +
    facet_wrap(vars(bety_name), scales = "free_y")
}

bety_species <- species_df %>%
  select(AccSpeciesName = scientificname, specie_id)

setDT(bety_species)
species_data_final <- bety_species[species_data_sub, on = "AccSpeciesName"]
species_data_final[, notes := paste0("TRY.ObservationID = ", ObservationID, "|",
                                     "TRY.DataID = ", DataID)]

# Exclude TRY data already in BETY
bety_try_data <- tbl(con, "traits") %>%
  filter(notes %like% "TRY%") %>%
  collect() %>%
  separate(notes, c("ObservationID", "DataID"), sep = "\\|") %>%
  mutate_at(vars(ObservationID, DataID), ~as.numeric(str_extract(., "[[:digit:]]+"))) %>%
  select(ends_with("ID"))

setDT(bety_try_data)
# data.table equivalent of `anti_join(species_data_final, bety_try_data)`
species_data_insert <- species_data_final[!bety_try_data,
                                          on = c("ObservationID", "DataID")]

ninsert <- dbExecute(con, paste(
  "INSERT INTO traits (variable_id, specie_id, mean, n, notes)",
  "VALUES ($1, $2, $3, 1, $4)"
), with(species_data_insert, list(id, specie_id, StdValue, notes)))

message("Inserted ", ninsert, " trait records.")

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
