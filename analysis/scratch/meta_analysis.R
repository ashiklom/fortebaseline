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

pfts <- paste0("umbs.", c(
  "early_hardwood",
  "mid_hardwood",
  "late_hardwood",
  "northern_pine"
))

PEcAn.logger::logger.setLevel("WARN")
outfile <- here::here("analysis", "data", "raw-data", "meta-analysis.rds")
if (!file.exists(outfile)) {
  ma_results <- map(pfts, pecan_ma_pft, con = con) %>% setNames(pfts)
  saveRDS(ma_results, outfile)
} else {
  ma_results <- readRDS(outfile)
}

posterior <- tidy_posterior(ma_results)

trait_draws %>%
  mutate(pft = factor(pft, pfts) %>% fct_relabel(~gsub("umbs.", "", .x))) %>%
  ggplot() +
  aes(x = pft, y = Mean, ymin = lo, ymax = hi) +
  geom_pointrange() +
  facet_wrap(vars(trait), scales = "free_y")
