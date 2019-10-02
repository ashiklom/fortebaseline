# Compare priors and default values
library(tidyverse)

prior <- read_csv("analysis/data/retrieved/pft-priors.csv")

defaults <- ed_default_params() %>%
  semi_join(prior, "trait") %>%
  select(pft = bety_name, trait, default_value)
dcalc <- function(distn, parama, paramb) {
  from_to <- rlang::exec(paste0("q", distn), c(0.025, 0.975), parama, paramb)
  x <- seq(from_to[1], from_to[2], length.out = 500)
  y <- rlang::exec(paste0("d", distn), x, parama, paramb)
  tibble::tibble(x, y)
}
prior_curves <- prior %>%
  select(pft:paramb) %>%
  mutate(calc = pmap(list(distn, parama, paramb), dcalc)) %>%
  unnest(calc)

prior_curves %>%
  mutate(pft = fct_inorder(pft)) %>%
  filter(trait == "mort3") %>%
  ggplot() +
  aes(x = x, y = y, color = pft, linetype = pft) +
  geom_line() +
  ## geom_vline(aes(xintercept = default_value, color = pft), data = defaults) +
  facet_wrap(vars(trait), scales = "free")

### Now, let's fix some priors
#### growth_resp_factor
prior %>%
  filter(trait == "growth_resp_factor")
defaults %>%
  filter(trait == "growth_resp_factor")
pbeta()
qbeta(0.5, 3, 5)
pbeta(0.450, 3, 3.6)
pbeta(0.450, 4.06, 7.2)

curve(dbeta(x, 5, 5), 0, 1)
abline(v = 0.45)

curve(dbeta(x, 4.06, 7.2), 0, 1)
curve(dbeta(x, 3, 3.6), 0, 1, col = 2, add = TRUE)
abline(v = 0.45)
#### leaf_respiration_rate_m2
prior %>%
  filter(trait == "leaf_respiration_rate_m2")
defaults %>%
  filter(trait == "leaf_respiration_rate_m2")

# EML: 1.32, 0.5, 1.97
pgamma(c(0.33, 0.66), 4, 8)
curve(dgamma(x, 4, 8), 0, 1)

#### leaf_turnover_rate
prior %>%
  filter(trait == "leaf_turnover_rate")
defaults %>%
  filter(trait == "leaf_turnover_rate")

#### mort2
prior %>% filter(trait == "mort2")
defaults %>% filter(trait == "mort2")
pgamma(20, 1.47, 0.0578)
curve(dgamma(x, 1.47, 0.0578), 0, 100)
#### mort3
defaults %>% filter(trait == "mort3")
#### root_respiration_rate
prior %>% filter(trait == "root_respiration_rate")
defaults %>% filter(trait == "root_respiration_rate")
pgamma(0.794, 4, 0.15)
pweibull(0.794, 2.66, 6.29)
plnorm(0.794, 2.07, 0.4)
curve(dweibull(x, 2.66, 6.29), 0, 30)
curve(dweibull(x, 2, 10), 0, 30)
curve(dgamma(x, 4.95, 0.762), 0, 30)
curve(d(x, 2.66, 6.29), 0, 30)
#### root_turnover_rate
prior %>% filter(trait == "root_turnover_rate")
defaults %>% filter(trait == "root_turnover_rate")
summary(rweibull(5000, 1.55, 0.862))
summary(rweibull(5000, 1.67, 0.657))
summary(rgamma(5000, 3, 2.5))

curve(dweibull(x, 1.55, 0.862), 0, 10)
curve(dweibull(x, 1.55, 1.5), 0, 10, add = TRUE, col = 2)
curve(dgamma(x, 2, 0.7), 0, 10)
#### stomatal_slope
prior %>% filter(trait == "stomatal_slope")
defaults %>% filter(trait == "stomatal_slope")
curve(dweibull(x, 7.11, 6.29), 0, 20)
curve(dlnorm(x, 2.3, 1), 0, 20)
#### Query BETY
con <- bety()
pgamma(5, 2, 0.7)

tbl(con, "variables") %>% filter(name == "stomatal_slope") %>% select(id, units)

tbl(con, "priors") %>%
  filter(variable_id == 26) %>%
  collect() %>%
  print(n = Inf)
