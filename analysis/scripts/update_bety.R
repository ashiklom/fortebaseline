#!/usr/bin/env Rscript
if (getRversion() >= "3.6") {
  options(conflicts.policy = "strict")
  conflictRules("testthat", exclude = c("matches", "is_null"))
  conflictRules("dplyr",
                mask.ok = c("filter", "lag", "intersect",
                            "setdiff", "setequal", "union"))
} else {
  warning("Package conflict resolution requires R >= 3.6. ",
          "This script may not work as expected.",
          immediate. = TRUE)
}

library(dplyr)
library(here)
library(fs)
library(yaml)
library(purrr)
library(readr)
library(assertthat)
library(readr)
devtools::load_all(here())

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
  add_variable_if_missing(variable, con = con)
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

delete_prior <- function(variable, pft = pfts("bety_name"), con = bety()) {
  priors <- pfts_priors(pft) %>%
    dplyr::filter(variable %in% !!variable)
  DBI::dbExecute(con, paste0(
    "DELETE FROM pfts_PRIORS WHERE ",
    "pft_id = $1 AND prior_id = $2"
  ), param = list(priors[["pft_id"]], priors[["prior_id"]]))
}

add_variable_if_missing <- function(variable, con = bety()) {
  existing <- dplyr::tbl(con, "variables") %>%
    dplyr::filter(name %in% !!variable) %>%
    dplyr::distinct(name) %>%
    dplyr::pull()
  new <- setdiff(variable, existing)
  if (length(new) > 0) {
    message("Adding the following new variable records:",
            paste(shQuote(new), collapse = ", "))
    insert <- DBI::dbExecute(con, paste0(
      "INSERT INTO variables (name, description) ",
      "VALUES ($1, 'FORTEBASELINE: Added automatically.')"
    ), params = list(new))
  }
  invisible(new)
}

structure_priors <- tribble(
  ~variable, ~distn, ~parama, ~paramb,
  # Clumping:
  # - 0 = black hole, 1 = perfectly even
  # - Prior based on my own expert judgment
  "clumping_factor", "beta", 3, 1.5,
  # Orientation factor:
  # - -1 = vertical, 1 = horizontal, 0 = random
  # - Prior based on Viskari et al. 2019 PLoS ONE
  "orient_factor", "unif", -0.5, 0.5
) %>%
  tidyr::crossing(pft = pfts("bety_name"))

other_priors <- tribble(
  ~pft, ~variable, ~distn, ~parama, ~paramb,
  # Carbon balance mortality
  # Based loosely on Raczka tuning values; middle 50% of mortality is
  # 3 to 25 year-1.
  "umbs.early_hardwood", "mort1", "gamma", 1, 0.05,
  "umbs.mid_hardwood", "mort1", "gamma", 1, 0.05,
  "umbs.late_hardwood", "mort1", "gamma", 1, 0.05,
  "umbs.northern_pine", "mort1", "gamma", 1, 0.05,
  # Same as Raczka late hardwood posterior. Assume same value for all
  # PFTs because Raczka list all PFTs having the same median.
  "umbs.early_hardwood", "mort3", "unif", 0, 0.02,
  "umbs.mid_hardwood", "mort3", "unif", 0, 0.02,
  "umbs.late_hardwood", "mort3", "unif", 0, 0.02,
  "umbs.northern_pine", "mort3", "unif", 0, 0.02,
  # Raczka late hardwood posterior, as above.
  "umbs.early_hardwood", "minimum_height", "gamma", 1.5, 0.2,
  "umbs.mid_hardwood", "minimum_height", "gamma", 1.5, 0.2,
  "umbs.late_hardwood", "minimum_height", "gamma", 1.5, 0.2,
  "umbs.northern_pine", "minimum_height", "gamma", 1.5, 0.2,
  # Pine leaf respiration prior was missing. This is an uninformative
  # prior for pine barrens
  "umbs.northern_pine", "leaf_respiration_rate_m2", "weibull", 2, 6
)

pft_definition <- here("analysis", "data", "derived-data", "pft_definition.yml") %>%
  read_yaml() %>%
  map_dfr(data.frame, stringsAsFactors = FALSE) %>%
  as_tibble()

p_pfts <- define_pfts(unique(pft_definition[["pft"]]))
p_pfts_species <- set_pft_species(pft_definition)
spec_priors_data <- read_csv(here("analysis", "data",
                                 "derived-data", "priors_spectra.csv"))

set_prior_l <- lift_dl(set_prior, overwrite = TRUE)
set_spec_priors <- set_prior_l(spec_priors_data)
set_structure_priors <- set_prior_l(structure_priors)
set_other_priors <- set_prior_l(other_priors)
remove_priors <- delete_prior(c(
  "c2n_fineroot",
  "c2n_leaf",
  "cuticular_cond",
  "leaf_turnover_rate",
  "leaf_width",
  "Vm_low_temp"
))

priors <- pfts_priors() %>%
  select(pft, trait = variable, distn, parama, paramb,
         pft_id, prior_id) %>%
  mutate(is_posterior = FALSE)

write_csv(priors, path(
  "analysis", "data", "derived-data", "pft-priors.csv"
))

# Run PEcAn meta-analysis
PEcAn.logger::logger.setLevel("WARN")
pfts <- pfts()
ma_results <- map(pfts[["bety_name"]], pecan_ma_pft, con = bety()) %>%
  setNames(pfts[["pft"]])
ma_outfile <- here("analysis", "data", "retrieved", "meta-analysis.rds")
saveRDS(ma_results, ma_outfile)
