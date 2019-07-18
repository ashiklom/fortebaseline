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
define_pfts <- function(pft_dict,
                        con = bety()) {
  pft_names <- pft_dict[["pft"]]
  based_on <- pft_dict[["based_on"]]
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
  result <- tbl(con, "pfts") %>%
    filter(name %in% !!pfts("bety_name")) %>%
    collect()
  dict2 <- result %>%
    left_join(pft_dict, c("name" = "pft")) %>%
    select(pft_id = id, target_pft = name, pft = based_on)

  # Copy any missing priors from the "based_on" PFTs
  add_priors <- pfts_priors(based_on, con, collect = TRUE) %>%
    select(pft, variable, prior_id) %>%
    inner_join(dict2, "pft") %>%
    anti_join(pfts_priors(pft_names, con, collect = TRUE),
              c("target_pft" = "pft", "variable"))
  if (nrow(add_priors) > 0) {
    q <- DBI::dbExecute(con, paste0(
      "INSERT INTO pfts_priors (pft_id, prior_id) ",
      "VALUES ($1, $2)"
    ), with(add_priors, list(pft_id, prior_id)))
  }

  result
}

# Set the PFT-species relationship in BETY
set_pft_species <- function(species_df, con = bety(),
                            dryrun = FALSE) {
  stopifnot(all(c("species", "pft") %in% colnames(species_df)))
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
                      con = bety(), dryrun = FALSE,
                      overwrite = FALSE) {
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
      ndelete <- DBI::dbExecute(con, paste0(
        "DELETE FROM pfts_priors WHERE ",
        "pft_id = $1 AND prior_id = $2"
      ), param = list(
        existing_priors[["pft_id"]],
        existing_priors[["prior_id"]]
      ))
      message("Because overwrite = TRUE, deleted ", ndelete, " existing priors.")
    }
  }
  if (overwrite) {
    new_priors <- input
  } else {
    new_priors <- input %>%
      anti_join(current_priors, by = c("variable", "variable_id", "pft", "pft_id"))
  }
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
  "umbs.northern_pine", "leaf_respiration_rate_m2", "weibull", 2, 6,
  # Water conductance prior is way too wide. This one is centered on
  # the default value, but isn't quite as ridiculously broad.
  "umbs.early_hardwood", "water_conductance", "lnorm", log(2e-5), 3.5,
  "umbs.mid_hardwood", "water_conductance", "lnorm", log(2e-5), 3.5,
  "umbs.late_hardwood", "water_conductance", "lnorm", log(2e-5), 3.5,
  "umbs.northern_pine", "water_conductance", "lnorm", log(2e-5), 3.5,
)

pft_definition <- here("analysis", "data", "derived-data", "pft_definition.yml") %>%
  read_yaml() %>%
  map_dfr(data.frame, stringsAsFactors = FALSE) %>%
  as_tibble()

p_pfts <- pft_definition %>%
  distinct(pft, based_on) %>%
  define_pfts()
p_pfts_species <- set_pft_species(pft_definition)
spec_priors_data <- read_csv(here("analysis", "data",
                                  "derived-data", "priors_spectra.csv"),
                             col_types = "ccddc")

set_prior_l <- lift_dl(set_prior, overwrite = TRUE)
set_spec_priors <- set_prior_l(spec_priors_data)
set_structure_priors <- set_prior_l(structure_priors)
set_other_priors <- set_prior_l(other_priors)
remove_priors <- delete_prior(c(
  "cuticular_cond",
  "leaf_width",
  "Vm_low_temp"
))

priors <- pfts_priors() %>%
  select(pft, trait = variable, distn, parama, paramb,
         pft_id, prior_id) %>%
  mutate(is_posterior = FALSE)

# Run PEcAn meta-analysis
PEcAn.logger::logger.setLevel("WARN")
pfts <- pfts()
ma_results <- map(pfts[["bety_name"]], pecan_ma_pft, con = bety()) %>%
  setNames(pfts[["pft"]])

# Post-process
ma_posterior <- ma_results %>%
  tidy_posterior() %>%
  mutate(is_posterior = TRUE)
ma_prior <- priors %>%
  select(bety_name = pft, one_of(colnames(ma_posterior))) %>%
  left_join(pfts(), by = "bety_name")
missing_posteriors <- ma_prior %>%
  anti_join(ma_posterior, by = c("pft", "trait")) %>%
  draw_traits() %>%
  mutate(is_posterior = FALSE)
trait_distribution <- ma_posterior %>%
  bind_rows(missing_posteriors)

if (interactive()) {
  library(ggplot2)
  trait_distribution %>%
    mutate(pft = factor(pft, pfts("pft"))) %>%
    tidyr::unnest(draws) %>%
    filter(draws < quantile(draws, 0.975),
           draws > quantile(draws, 0.025)) %>%
    ggplot() +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin() +
    facet_wrap(vars(trait), scales = "free_y") +
    scale_fill_manual(values = pfts("color")) +
    labs(x = "PFT", fill = "PFT") +
    cowplot::theme_cowplot() +
    theme(axis.title.y = element_blank(),
          axis.text.x = element_blank())
}

# Store results
write_csv(priors, path(
  "analysis", "data", "derived-data", "pft-priors.csv"
))
ma_outdir <- dir_ls(here("analysis", "data", "retrieved"))
saveRDS(ma_results, path(ma_outdir, "meta-analysis.rds"))
saveRDS(trait_distribution, path(ma_outdir, "trait-distribution.rds"))
