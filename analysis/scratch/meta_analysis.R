library(tidyverse)
library(RPostgres)

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

np_results <- map(pfts, pecan_ma_pft, con = con)
names(np_results) <- pfts

np_summaries <- map_dfr(np_results, summarize_ma, .id = "pft") %>%
  mutate(pft = fct_relabel(pft, ~gsub("umbs.", "", .x)))

ggplot(np_summaries) +
  ## aes(x = pft, y = beta.o, ymin = beta.o - sd.y, ymax = beta.o + sd.y) +
  aes(x = pft, y = beta.o) +
  ## geom_pointrange() +
  geom_point() +
  facet_wrap(vars(trait), scales = "free")
