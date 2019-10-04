library(tidyverse)
library(RPostgres)
library(fortebaseline)

con <- bety()

pft <- "umbs.northern_pine"
pft_id <- PEcAn.DB::db.query("SELECT id FROM pfts WHERE name = $1",
                             con, values = list(pft))[[1]]
species <- PEcAn.DB::query.pft_species(pft, con = con)
priors <- PEcAn.DB::query.priors(pft_id, con = con)
trait_data <- PEcAn.DB::query.traits(
  species[["id"]],
  rownames(priors),
  con = con
)

sla <- trait_data$SLA
ggplot(sla) +
  aes(x = 1, y = mean_unconverted) +
  geom_jitter()

ma_raw <- pecan_ma_pft(con, pft)

ma_raw$posterior %>%
  filter(trait == "SLA")

curve(dgamma(x, 2, 0.2), 0, 15)
rug(sla$mean)

sla_post <- ma_raw$posterior %>%
  filter(trait == "SLA")
curve(dnorm(x, sla_post$parama, sla_post$paramb), 0, 15, add = TRUE)

# But we know there's more variability!
pnorm(2) - pnorm(-2)
