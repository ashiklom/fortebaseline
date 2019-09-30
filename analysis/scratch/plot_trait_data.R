library(tidyverse)
library(RPostgres)
library(fortebaseline)

con <- bety()

pfts <- paste0("umbs.", c(
  "early_hardwood",
  "mid_hardwood",
  "late_hardwood",
  "northern_pine"
))

l <- list()
for (pft in pfts) {
  pft_id <- PEcAn.DB::db.query("SELECT id FROM pfts WHERE name = $1",
                               con, values = list(pft))[[1]]
  stopifnot(length(pft_id) == 1)
  species <- PEcAn.DB::query.pft_species(pft, con = con)
  priors <- PEcAn.DB::query.priors(pft_id, con = con)
  trait_data <- PEcAn.DB::query.traits(
    species[["id"]],
    rownames(priors),
    con = con
  )
  l[[pft]] <- trait_data
}

spp <- tbl(con, "species") %>%
  filter(id %in% !!unique(all_traits$specie_id)) %>%
  select(specie_id = id, scientificname) %>%
  collect()

all_traits <- l %>%
  map(bind_rows, .id = "trait") %>%
  bind_rows(.id = "pft") %>%
  as_tibble() %>%
  left_join(spp, "specie_id")

plt <- all_traits %>%
  mutate(pft = factor(pft, pfts("bety_name"), pfts("shortname")),
         trait = recode(trait,
                        leaf_respiration_rate_m2 = "Leaf resp.",
                        root_respiration_rate = "Root resp.",
                        leaf_turnover_rate = "Leaf turnover",
                        clumping_factor = "Clump. fact.",
                        quantum_efficiency = "Quant. eff.")) %>%
  ggplot() +
  aes(x = scientificname, y = mean) +
  ## geom_violin() +
  geom_jitter(size = 0.2, width = 0.1, alpha = 0.6) +
  facet_grid(vars(trait), vars(pft), scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("analysis/figures/trait-data.png", plt, width = 7, height = 9.5)
