# begin imports
import::from("RPostgres", "Postgres", .into = "")
import::from("DBI", "dbConnect", .into = "")
import::from("tibble", "tribble", .into = "")
# end imports

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

# Begin example
## con <- PEcAn.DB::db.open(...)
## if (FALSE) {
##   pft <- "temperate.Early_Hardwood"
##   pft_id <- PEcAn.DB::db.query("SELECT id FROM pfts WHERE name = $1", con,
##                                values = list(pft))[[1]]
##   traits <- c("SLA", "Vcmax")
##   trait_string <- paste(shQuote(traits), collapse = ",")

##   species <- PEcAn.DB::query.pft_species(pft, con = con)
##   trait.data <- PEcAn.DB::query.traits(species[["id"]], c("SLA", "Vcmax"), con = con)
##   prior.distns <- PEcAn.DB::query.priors(pft_id, trait_string, con = con)

##   jagged.data <- lapply(trait.data, PEcAn.MA::jagify)
##   taupriors <- list(tauA = 0.01,
##                     tauB = c(SLA = 1000, Vcmax = 1000))
##   result <- pecan.ma(jagged.data, prior.distns, taupriors,
##                      j.iter = 5000, outdir = tempdir(),
##                      logfile = NULL)
## }
# End example

pft <- "umbs.early_hardwood"
pft_id <- PEcAn.DB::db.query("SELECT id FROM pfts WHERE name = $1", con, values = list(pft))[[1]]
species <- PEcAn.DB::query.pft_species("umbs.early_hardwood", con = con)
trait_data <- PEcAn.DB::query.traits(
  species[["id"]],
  bety2try[["bety_name"]],
  con = con
)
jagged_data <- lapply(trait_data, PEcAn.MA::jagify)
priors <- PEcAn.DB::query.priors(
  pft_id,
  trtstr = bety2try[["bety_name"]],
  con = con
)
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
