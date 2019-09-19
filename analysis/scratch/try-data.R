library(data.table, exclude = c("between", "first", "last"))
library(magrittr, include.only = "%>%")
library(fst)
library(dplyr, mask.ok = c("filter", "lag", "intersect",
                           "setdiff", "setequal", "union"))
library(tidyr)
library(stringr)

library(fortebaseline)

pft_sp <- pfts_species()

pft_spp <- unique(pft_sp$scientificname)

bety2try <- tibble::tribble(
  ~bety_name, ~DataID,
  "SLA", 6582,
  ## "c2n_leaf", 455,
  ## "c2n_fineroot", 489,
  ## "leaf_width", 447,
  "Vcmax", 550,
  "fineroot2leaf", 5383,
  "leaf_respiration_rate_m2", 69,
  "root_respiration_rate", 1189
)

fst_file <- "~/Projects/try-raw-data/4143.fst"
try_fst <- fst(fst_file)
ispp <- try_fst$AccSpeciesName %in% pft_spp
idata <- try_fst$DataID %in% bety2try$DataID
try_vars <- c("AccSpeciesName", "ObservationID",
              "DataName", "DataID", "StdValue",
              "ValueKindName")
trydata <- try_fst[ispp & idata, try_vars]
setDT(trydata)

# Populus tremuloides has relatively low SLA, in BETY and in TRY
ipop <- grepl("Populus tremul", try_fst$AccSpeciesName)
pop <- try_fst[ipop & try_fst$DataID == 6582, try_vars]
setDT(pop)
hist(pop$StdValue)

dat <- as.data.table(bety2try)[trydata, on = "DataID"]

library(ggplot2)
dat[bety_name == "SLA"] %>%
  ggplot() +
  aes(x = AccSpeciesName, y = StdValue) +
  geom_violin() +
  geom_jitter(size = 1, alpha = 0.5)

con <- bety()

bety_traits <- tbl(con, "species") %>%
  filter(scientificname %in% pft_spp) %>%
  distinct(id, scientificname) %>%
  inner_join(tbl(con, "traits"), c("id" = "specie_id")) %>%
  inner_join(tbl(con, "variables") %>% filter(name == "SLA"),
             c("variable_id" = "id")) %>%
  select(species = scientificname, sla = mean) %>%
  collect()

bety_traits %>%
  ggplot() +
  aes(x = species, y = sla) +
  geom_violin() +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_hline(yintercept = c(15, 20))


dat %>%
  as_tibble() %>%
  mutate(source = "TRY") %>%
  left_join(bety_traits, c(""))
