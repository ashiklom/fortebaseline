if (getRversion() >= "3.6") {
  options(conflicts.policy = "strict")
  conflictRules("dplyr", mask.ok = c(
    "filter", "lag", "intersect",
    "setdiff", "setequal", "union"
  ))
} else {
  warning("Package conflict resolution requires R >= 3.6. ",
          "This script may not work as expected.",
          immediate. = TRUE)
}
library(fortebaseline)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(yaml, include.only = "read_yaml")
library(here, include.only = "here")
library(PEcAnRTM, include.only = "prospect")
library(readr, include.only = "write_csv")

pft_definition <- read_yaml(here(
  "analysis", "data", "derived-data", "pft_definition.yml"
)) %>%
  map_dfr(data.frame, stringsAsFactors = FALSE) %>%
  as_tibble()

bety_code <- tbl(bety(), "species") %>%
  select(scientificname, Symbol) %>%
  filter(scientificname %in% !!pft_definition$species) %>%
  collect()

pft_spp_codes <- pft_definition %>%
  left_join(bety_code, by = c("species" = "scientificname"))

f <- file.path("~", "laptop_folders", "Projects", "prospect-traits",
               "rspecan", "spectra_db", "cleaned_wide.rds")
stopifnot(file.exists(f))
d <- readRDS(f)

dsub <- d %>%
  filter(prospect_version == "D") %>%
  select(project_code, observation_id, species_code,
         N, Cab, Car, Canth, Cbrown, Cw, Cm) %>%
  inner_join(pft_spp_codes, by = c("species_code" = "Symbol"))

# All
prospectd <- lift_vl(prospect, version = "D")# %>% compose(list, .dir = "forward")
prospectd(list(1.4, 40, 10, 5, 0, 0.01, 0.01))

sims <- dsub %>%
  nest(N:Cm) %>%
  mutate(prospect_sim = map(data, prospectd))

vis <- 400:700
nir <- 700:1400

sims_sub <- sims %>%
  mutate(
    leaf_reflect_vis = map_dbl(prospect_sim, ~mean(.[[vis]][, "reflectance"], na.rm = TRUE)),
    leaf_reflect_nir = map_dbl(prospect_sim, ~mean(.[[nir]][, "reflectance"], na.rm = TRUE)),
    leaf_trans_vis = map_dbl(prospect_sim, ~mean(.[[vis]][, "transmittance"], na.rm = TRUE)),
    leaf_trans_nir = map_dbl(prospect_sim, ~mean(.[[nir]][, "transmittance"], na.rm = TRUE))
  ) %>%
  select(-prospect_sim)

sims_fit <- sims_sub %>%
  group_by(pft) %>%
  summarize_at(vars(starts_with("leaf_")), compose(list, safely(fitdistrplus::fitdist)),
               distr = "beta") %>%
  gather(variable, value, -pft) %>%
  mutate(
    error = map(value, "error"),
    result = map(value, "result"),
    estimate = map(result, "estimate"),
    shape1 = map_dbl(estimate, 1),
    shape2 = map_dbl(estimate, 2)
  ) %>%
  select(pft, variable, parama = shape1, paramb = shape2) %>%
  mutate(distn = "beta")

write_csv(sims_fit, here("analysis", "data", "derived-data", "priors_spectra.csv"))

if (interactive()) {
  sims_in <- sims_sub %>%
    gather(variable, value, starts_with("leaf_"))

  sims_beta <- sims_fit %>%
    mutate(
      x = list(seq(0, 1, 0.01)),
      fun = map2(parama, paramb, ~partial(dbeta, shape1 = .x, shape2 = .y)),
      pred = map2(x, fun, ~.y(.x))
    ) %>%
    unnest(x, pred)

  ggplot() +
    aes(color = pft) +
    geom_density(aes(x = value, linetype = "data"), data = sims_in) +
    geom_line(aes(x = x, y = pred, linetype = "fit"), data = sims_beta) +
    scale_linetype_manual(values = c("data" = "dotted", "fit" = "solid")) +
    facet_wrap(vars(variable), scales = "free_y")

}
