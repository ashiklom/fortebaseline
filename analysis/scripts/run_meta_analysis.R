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

PEcAn.logger::logger.setLevel("WARN")
outfile <- here::here("analysis", "data", "derived-data", "meta-analysis.rds")
pfts <- pfts()
ma_results <- map(pfts[["bety_name"]], pecan_ma_pft, con = con) %>%
  setNames(pfts[["pft"]])
saveRDS(ma_results, outfile)
