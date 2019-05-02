#!/usr/bin/env Rscript
if (getRversion() >= "3.6") {
  options(conflicts.policy = "strict")
  conflictRules("testthat", exclude = c("matches", "is_null"))
  conflictRules("drake", exclude = c("gather", "expand", "plan"))
  conflictRules("dplyr",
                mask.ok = c("filter", "lag", "intersect",
                            "setdiff", "setequal", "union"))
} else {
  warning("Package conflict resolution requires R >= 3.6. ",
          "This script may not work as expected.",
          immediate. = TRUE)
}

library(drake)
library(dplyr)
library(here)
library(fs)
library(yaml)
library(purrr)
library(readr)
library(assertthat)
devtools::load_all(here())

cache_path <- dir_create(here(".drake_bety"))
cache <- new_cache(path = cache_path)
stopifnot(cache$driver$path == cache_path)

# Make sure we can connect to bety
invisible(bety())

# Assign plant functional types in BETY
define_pfts <- function(pft_names, con = bety()) {
  stopifnot(setequal(pft_names, pfts("bety_name")))
  inbety <- tbl(con, "pfts") %>%
    filter(name %in% pft_names) %>%
    collect()
  input <- tibble(name = pft_names)
  insert <- anti_join(input, inbety, by = "name")
  if (nrow(insert) > 1) {
    s <- DBI::dbSendStatement(con, paste0(
      "INSERT INTO pfts (name) VALUES ($1)"
    ))
    q <- DBI::dbBind(s, unname(as.list(insert)))
    DBI::dbClearResult(q)
  }
  tbl(con, "pfts") %>%
    filter(name %in% !!pfts("bety_name")) %>%
    collect()
}

# Set the PFT-species relationship in BETY
set_pft_species <- function(species_df, con = bety(), dryrun = FALSE) {
  stopifnot(setequal(c("species", "pft"), colnames(species_df)))
  input_species <- species_df[["species"]]
  input_pfts <- unique(species_df[["pft"]])
  bety_species <- tbl(con, "species") %>%
    filter(scientificname %in% input_species) %>%
    select(specie_id = id, species = scientificname) %>%
    collect()
  if (!setequal(bety_species[["species"]], input_species)) {
    msg <- sprintf(paste0(
      "Mismatch between bety and input species.\n",
      "`setdiff(bety, input)` gives: %s .\n",
      "`setdiff(input, bety)` gives: %s ."
    ),
    paste(setdiff(bety_species[["species"]], input_species), collapse = ", "),
    paste(setdiff(input_species, bety_species[["species"]]), collapse = ", ")
    )
    stop(msg)
  }
  input <- rename(species_df, scientificname = species)
  inbety <- pfts_species() %>%
    filter(scientificname %in% input_species) %>%
    collect()
  insert <- input %>%
    anti_join(inbety, by = c("pft", "scientificname"))
  if (nrow(insert) > 0) {
    message("Inserting ", nrow(insert), " new PFT-species records.")
    if (!dryrun) {
      s <- DBI::dbSendStatement(con, paste0(
        "INSERT INTO pfts_species (pft_id, specie_id) VALUES ($1, $2)"
      ))
      q <- DBI::dbBind(s, list(
        get_pft_ids(insert[["pft"]]),
        get_species_ids(insert[["scientificname"]])
      ))
      DBI::dbClearResult(s)
    }
  }
  remove <- pfts_species(con = con) %>%
    filter(pft %in% !!unique(input[["pft"]])) %>%
    select(pft_id, pft, specie_id, scientificname) %>%
    collect() %>%
    anti_join(input, by = c("pft", "scientificname"))
  if (nrow(remove) > 0) {
    message("Deleting ", nrow(remove), " PFT-species records.")
    if (!dryrun) {
      s <- DBI::dbSendStatement(con, paste0(
        "DELETE FROM pfts_species WHERE pft_id = $1 AND specie_id = $2"
      ))
      q <- DBI::dbBind(s, unname(as.list(remove[, c("pft_id", "specie_id")])))
      DBI::dbClearResult(s)
    }
  }
  pfts_species(input_pfts) %>% collect()
}

set_prior <- function(variable, distn, parama, paramb, pft,
                      con = bety(), overwrite = TRUE, dryrun = FALSE) {
  # Get variable IDs
  variable_id <- get_variable_ids(variable, con)
  pft_id <- get_pft_ids(pft, con)
  # Add any missing variables
  input <- tidyr::nesting(variable, variable_id, distn, parama, paramb, pft, pft_id)
  # Check for existing prior
  current_priors <- pfts_priors(pft)
  existing_priors <- current_priors %>%
    semi_join(input, by = c("variable", "variable_id", "pft", "pft_id"))
  if (nrow(existing_priors) > 0) {
    message("Found ", nrow(existing_priors), " existing priors.")
    if (overwrite && !dryrun) {
      # Delete it
      qry <- DBI::dbSendStatement(con, paste0(
        "DELETE FROM pfts_priors WHERE ",
        "pft_id = $1 AND prior_id = $2"
      ))
      stmt <- DBI::dbBind(qry, list(
        existing_priors[["pft_id"]],
        existing_priors[["prior_id"]]
      ))
      DBI::dbClearResult(qry)
    }
  }
  new_priors <- input %>%
    anti_join(current_priors, by = c("variable", "variable_id", "pft", "pft_id"))
  if (nrow(new_priors) > 0) {
    message("Inserting ", nrow(new_priors), " new priors.")
    if (!dryrun) {
      .success <- DBI::dbWithTransaction(con, {
        qry <- DBI::dbSendQuery(con, paste0(
          "INSERT INTO priors (variable_id, distn, parama, paramb, phylogeny) ",
          "VALUES ($1, $2, $3, $4, 'plants') ",
          "RETURNING id, variable_id, distn, parama, paramb"
        ))
        res <- DBI::dbBind(qry, with(new_priors, list(
          variable_id, distn, parama, paramb
        )))
        prior_insert <- DBI::dbFetch(res)
        DBI::dbClearResult(qry)
        qry <- DBI::dbExecute(con, paste0(
          "INSERT INTO pfts_priors (pft_id, prior_id) ",
          "VALUES ($1, $2) "
        ), list(new_priors[["pft_id"]], prior_insert[["id"]]))
      })
    }
  }
  pfts_priors(pft)
}

plan <- drake_plan(
  pft_definition = read_yaml(
    file_in(!!here("analysis", "data", "derived-data", "pft_definition.yml"))
  ) %>%
    map_dfr(data.frame, stringsAsFactors = FALSE) %>%
    as_tibble(),
  p_pfts = define_pfts(unique(pft_definition[["pft"]])),
  p_pfts_species = set_pft_species(pft_definition),
  spec_priors_data = read_csv(file_in(!!here("analysis", "data",
                                             "derived-data", "priors_spectra.csv"))),
  set_spec_priors = lift_dl(set_prior)(spec_priors_data)
)

dconf <- drake_config(
  plan,
  cache = cache,
  prework = paste0("devtools::load_all(",
                   "here::here(), ",
                   "quiet = TRUE)")
)

if (interactive()) {
  self <- here("analysis", "scripts", "bety_plan.R")
  callr::rscript(self)
  loadd(cache = cache)
} else {
  make(config = dconf)
}
