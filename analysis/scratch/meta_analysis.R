library(tidyverse)
library(RPostgres)

con <- dbConnect(
  Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

bety2try <- tribble(
  ~bety_name, ~DataID,
  "SLA", 6582,
  "c2n_leaf", 455,
  "c2n_fineroot", 489,
  "Vcmax", 550,
  "fineroot2leaf", 5383,
  "leaf_width", 447,
  "root_respiration_rate", 1189
)

pft <- "umbs.northern_pine"
pft_id <- PEcAn.DB::db.query("SELECT id FROM pfts WHERE name = $1", con, values = list(pft))[[1]]
species <- PEcAn.DB::query.pft_species("umbs.early_hardwood", con = con)

priors <- PEcAn.DB::query.priors(
  pft_id,
  ## trtstr = bety2try[["bety_name"]],
  con = con
)

trait_data <- PEcAn.DB::query.traits(
  species[["id"]],
  rownames(priors),
  con = con
)
jagged_data <- lapply(trait_data, PEcAn.MA::jagify)
taupriors <- list(
  tauA = 0.01,
  tauB = setNames(rep(0.01, nrow(priors)), rownames(priors))
)
outdir <- tempdir()

ma_result <- PEcAn.MA::pecan.ma(
  jagged_data, priors,
  taupriors = taupriors,
  j.iter = 3000,
  outdir = outdir,
  logfile = NULL
)

summarize_ma <- function(result) {
  smry <- summary(result)
  dfs <- purrr::map(smry[1:2], tibble::as_tibble, rownames = "variable")
  dplyr::left_join(dfs[[1]], dfs[[2]], by = "variable")
}

ma_summary <- map_dfr(ma_result, summarize_ma, .id = "pft")
ma_traits <- ma_summary %>%
  select(pft, variable, Mean) %>%
  filter(variable %in% c("beta.o", "sd.y")) %>%
  spread(variable, Mean)
ma_traits
