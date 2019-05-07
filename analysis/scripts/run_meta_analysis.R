options(conflicts.policy = "depends.ok")
library(tidyverse)
library(RPostgres)
library(fortebaseline)

con <- dbConnect(
  Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 7990
)

# NOTE: This will silently drop parameters that don't have a prior (I
# think?). Need to figure out why that is.
PEcAn.logger::logger.setLevel("WARN")
outfile <- here::here("analysis", "data", "derived-data", "meta-analysis.rds")
pfts <- pfts()
ma_results <- map(pfts[["bety_name"]], pecan_ma_pft, con = con) %>%
  setNames(pfts[["pft"]])

if (interactive()) {
  tidy_posterior(ma_results) %>%
    unnest(draws) %>%
    mutate(pft = factor(pft, pfts[["pft"]])) %>%
    ggplot() +
    aes(x = pft, y = draws, fill = pft) +
    geom_violin() +
    facet_wrap(vars(trait), scales = "free")
}

saveRDS(ma_results, outfile)
