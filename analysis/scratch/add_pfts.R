import::from(magrittr, "%>%")
import::from(DBI, dbConnect)
import::from(RPostgres, Postgres)
import::from(tibble, tibble, tribble)
import::from(dplyr, mutate, case_when, tbl, inner_join, semi_join, select, filter, collect)
import::from(pecanapi, prepared_statement)

con <- dbConnect(
  Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

## DBI::dbListTables(con) %>%
##   grep(pattern = "pft", value = TRUE)

species_list <- file.path("analysis", "data", "derived-data", "species_list.txt") %>%
  readLines()

species_df <- tibble(species = species_list) %>%
  mutate(
    pft = case_when(
      grepl("Betula|Populus", species) ~ "umbs.early_hardwood",
      grepl("Quercus rubra|Acer rubrum", species) ~ "umbs.mid_hardwood",
      grepl("Acer saccharum|Fagus", species) ~ "umbs.late_hardwood",
      grepl("Pinus", species) ~ "umbs.northern_pine",
      TRUE ~ NA_character_
    )
  )

if (FALSE) {
  dplyr::tbl(con, "pfts") %>%
    dplyr::filter(name %like% "umbs%")
}

# Create PFT
pfts <- unique(species_df[["pft"]])

qry <- prepared_statement(con, paste0(
  "INSERT INTO pfts (name, pft_type, modeltype_id, definition) ",
  "VALUES ($1, 'plant', 1, 'Species observed at UMBS')"
), list(pfts))

# Add species to PFT
species_ids <- tbl(con, "species") %>%
  select(specie_id = id, species = scientificname) %>%
  inner_join(species_df, copy = TRUE) %>%
  collect()

pft_ids <- tbl(con, "pfts") %>%
  filter(name %like% "umbs%") %>%
  select(pft_id = id, pft = name) %>%
  collect()

pft_species <- species_ids %>%
  inner_join(pft_ids)

prepared_statement(con, paste0(
  "INSERT INTO pfts_species (pft_id, specie_id) ",
  "VALUES ($1, $2)"
), list(pft_species[["pft_id"]], pft_species[["specie_id"]]))

# Copy priors from reference PFT
pft_reference <- tribble(
  ~pft, ~reference,
  "umbs.early_hardwood", "temperate.Early_Hardwood",
  "umbs.mid_hardwood", "temperate.North_Mid_Hardwood",
  "umbs.late_hardwood", "temperate.Late_Hardwood",
  "umbs.northern_pine", "temperate.Northern_Pine"
)

tbl(con, "inputs") %>%
  dplyr::group_by(site_id, format_id) %>%
  dplyr::count(sort = TRUE) %>%
  filter(n > 1)

site_id <- 1000000033
tbl(con, "inputs") %>%
  filter(site_id == !!site_id, format_id == 33) %>%
  select(id, name, start_date, end_date)

tbl(con, "formats") %>% filter(id == 33) %>% dplyr::glimpse()
  filter(container_type == "Input", file_name == "CRUNCEP") %>%
  dplyr::collect() %>%
  select(id, file_name, file_path)

tbl(con, "pfts") %>%
  filter(name == "temperate.Northern_Pine") %>%
  left_join()

## tbl(con, "pfts_species")
## tbl(con, "pfts_priors")

