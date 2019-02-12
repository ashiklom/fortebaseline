library(tidyverse)
library(fst)

ed_in <- fst::read_fst("analysis/data/derived-data/ed-ensemble-out.fst")

ed_summary <- ed_in %>%
  mutate(rundir = as.integer(factor(rundir))) %>%
  # Growing season
  filter(lubridate::month(time) >= 6, lubridate::month(time) <= 8) %>%
  ## group_by(variable, ymonth = lubridate::floor_date(time, unit = "month")) %>%
  group_by(rundir, variable, yyear = lubridate::year(time)) %>%
  summarize(
    value_mean = mean(value, na.rm = TRUE)
    ## value_lo = quantile(value, 0.025, na.rm = TRUE),
    ## value_hi = quantile(value, 0.975, na.rm = TRUE)
  ) %>%
  ungroup(ed_summary)

ggplot(ed_summary) +
  ## aes(x = ymonth, y = value_mean) +
  aes(x = yyear, y = value_mean, group = factor(rundir)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~variable, scales = "free_y")
ggsave("analysis/figures/prelim-ed-ensemble-out.png",
       width = 8, height = 8)
