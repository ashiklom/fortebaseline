library(ggplot2)

# begin imports
import::from("magrittr", "%>%", .into = "")
import::from("DBI", "dbConnect", .into = "")
import::from("RPostgres", "Postgres", .into = "")
import::from("dplyr", "tbl", "inner_join", "select", "filter", "collect", "pull", "count", "group_by", "summarize", "if_else", "mutate", "left_join", "distinct", "semi_join", "ungroup", .into = "")
import::from("purrr", "pmap_dbl", "exec", .into = "")
# end imports

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

pft_species_sql <- tbl(con, "pfts") %>%
  filter(name %in% pfts) %>%
  inner_join(tbl(con, "pfts_species"),
             by = c("id" = "pft_id"),
             suffix = c(".pft", ".ps")) %>%
  inner_join(tbl(con, "species"),
             by = c("specie_id" = "id"),
             suffix = c(".ps", ".species")) %>%
  select(id.pft, name, pft_type,
         specie_id, scientificname, Symbol)
pft_species <- collect(pft_species_sql)

species_file <- file.path("analysis", "data",
                          "derived-data", "species_list.txt")

if (!file.exists(species_file)) {
  writeLines(pft_species[["scientificname"]], species_file)
}

trait_data_sql <- pft_species_sql %>%
  inner_join(tbl(con, "traits"), by = "specie_id") %>%
  inner_join(tbl(con, "variables"),
             by = c("variable_id" = "id"),
             suffix = c(".value", ".variable"))

trait_data <- collect(trait_data_sql) %>%
  select(
    pft = name.value, pft_id = id.pft,
    species_id = specie_id, species = Symbol, scientificname,
    trait = name.variable, trait_id = variable_id,
    n, mean, statname, stat, max, min,
    units, standard_name, standard_units
  ) %>%
  mutate(n = if_else(is.na(n), 1L, n))

# Summary table by PFT
trait_summary <- trait_data %>%
  group_by(pft, pft_id, trait) %>%
  summarize(
    pft_n = sum(n, na.rm = TRUE),
    pft_mean = weighted.mean(mean, n, na.rm = TRUE),
    pft_sd = sqrt(sum((mean - pft_mean) ^ 2, na.rm = TRUE) / (pft_n - 1)),
    pft_min = min(mean, na.rm = TRUE),
    pft_max = max(mean, na.rm = TRUE)
  ) %>%
  ungroup()

pft_ids <- with(trait_data, unique(pft_id))

bety_priors <- tbl(con, "priors") %>%
  inner_join(tbl(con, "pfts_priors") %>% filter(pft_id %in% pft_ids),
             by = c("id" = "prior_id"),
             suffix = c(".priors", ".pfts_priors")) %>%
  select(
    pft_id, trait_id = variable_id,
    distn, parama, paramb, paramc, n
  ) %>%
  collect()

priors <- trait_data %>%
  distinct(pft, pft_id, trait, trait_id) %>%
  inner_join(bety_priors)

prior_stats <- priors %>%
  mutate(
    qfun = paste0("q", distn),
    lo = pmap_dbl(list(qfun, parama, paramb), exec, p = 0.025),
    mid = pmap_dbl(list(qfun, parama, paramb), exec, p = 0.5),
    hi = pmap_dbl(list(qfun, parama, paramb), exec, p = 0.975)
  ) %>%
  select(pft, pft_id, trait, trait_id, lo, mid, hi)

pft_labeller <- function(x) {
  x <- gsub("umbs.", "", x)
  x <- gsub("hardwood|northern", "", x)
  x <- gsub("_", "", x)
  x
}

trait_data %>%
  semi_join(prior_stats) %>%
  ggplot() +
  aes(x = pft, y = mean) +
  geom_violin() +
  geom_jitter() +
  geom_point(aes(y = mid), data = prior_stats, color = "red") +
  geom_errorbar(aes(y = NULL, ymin = lo, ymax = hi), data = prior_stats, color = "red") +
  facet_wrap(vars(trait), scales = "free_y") +
  scale_x_discrete(labels = pft_labeller)
