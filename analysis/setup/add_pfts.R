library(fortebaseline)

# begin imports
import::from("magrittr", "%>%", .into = "")
import::from("DBI", "dbConnect", .into = "")
import::from("RPostgres", "Postgres", .into = "")
import::from("tibble", "tibble", "tribble", .into = "")
import::from("dplyr", "mutate", "case_when", "tbl", "inner_join", "semi_join", "select", "filter", "collect", "pull", "left_join", "count", .into = "")
import::from("pecanapi", "prepared_statement", .into = "")
# end imports

con <- bety()

species_list <- file.path("analysis", "data",
                          "derived-data", "species_list.txt") %>%
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

# Create PFT
pfts <- unique(species_df[["pft"]])
bety_pfts <- tbl(con, "pfts") %>%
  filter(name %in% pfts) %>%
  collect()

if (nrow(bety_pfts) < 1) {
  qry <- prepared_statement(con, paste0(
    "INSERT INTO pfts (name, pft_type, modeltype_id, definition) ",
    "VALUES ($1, 'plant', 1, 'Species observed at UMBS')"
  ), list(pfts))
} else if (!all(pfts %in% bety_pfts[["name"]])) {
  stop("Only some PFTs are missing. Debug this manually.")
}

# Add species to PFT

pft_ids <- tbl(con, "pfts") %>%
  filter(name %in% pfts) %>%
  select(pft_id = id, pft = name) %>%
  collect()

bety_pft_species <- tbl(con, "pfts") %>%
  filter(name %in% pfts) %>%
  inner_join(tbl(con, "pfts_species"),
            by = c("id" = "pft_id"),
            suffix = c(".pfts", ".pfts_species")) %>%
  collect()

if (nrow(bety_pft_species) < 1) {
  species_ids <- tbl(con, "species") %>%
    select(specie_id = id, species = scientificname) %>%
    inner_join(species_df, copy = TRUE) %>%
    collect()

  pft_species <- species_ids %>%
    inner_join(pft_ids)

  prepared_statement(con, paste0(
    "INSERT INTO pfts_species (pft_id, specie_id) ",
    "VALUES ($1, $2)"
  ), list(pft_species[["pft_id"]], pft_species[["specie_id"]]))
} else if (nrow(bety_pft_species) != nrow(species_df)) {
  stop("BETY has ", nrow(bety_pft_species), " rows, but expected ",
       nrow(species_df), " rows. Debug this manually.")
}

# Copy priors from reference PFT
pft_reference <- tribble(
  ~pft, ~reference,
  "umbs.early_hardwood", "temperate.Early_Hardwood",
  "umbs.mid_hardwood", "temperate.North_Mid_Hardwood",
  "umbs.late_hardwood", "temperate.Late_Hardwood",
  "umbs.northern_pine", "temperate.Northern_Pine"
)

bety_ref_pfts <- tbl(con, "pfts") %>%
  filter(name %in% pft_reference[["reference"]]) %>%
  collect() %>%
  left_join(pft_reference, by = c("name" = "reference")) %>%
  left_join(pft_ids, by = "pft")

bety_pfts_priors <- tbl(con, "pfts_priors") %>%
  filter(pft_id %in% bety_ref_pfts[["id"]]) %>%
  collect() %>%
  inner_join(bety_ref_pfts,
             by = c("pft_id" = "id"),
             suffix = c(".bety", ".reference"))

# Check for duplicates
ndup <- bety_pfts_priors %>%
  count(pft_id, prior_id) %>%
  filter(n > 1) %>%
  nrow()
stopifnot(ndup == 0)

insert_priors <- bety_pfts_priors %>%
  select(pft_id = pft_id.reference, pft, prior_id)

# Check if priors already exist in database
bety_pft_prior <- tbl(con, "pfts_priors") %>%
  filter(pft_id %in% insert_priors[["pft_id"]],
         prior_id %in% insert_priors[["prior_id"]]) %>%
  collect()

if (nrow(bety_pft_prior) == 0) {
  qry <- prepared_statement(con, paste0(
    'INSERT INTO "pfts_priors" (pft_id, prior_id) ',
    "VALUES ($1, $2)"
  ), with(insert_priors, list(pft_id, prior_id)))
} else if (nrow(bety_pft_prior) != nrow(insert_priors)) {
  stop("BETY table has ", nrow(bety_pft_prior), " rows, but ",
       "local table has ", nrow(insert_priors), " rows. ",
       "Debug this manually.")
}
