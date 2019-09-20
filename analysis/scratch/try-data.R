library(data.table, exclude = c("between", "first", "last"))
library(magrittr, include.only = "%>%")
library(fst)
library(dplyr, mask.ok = c("filter", "lag", "intersect",
                           "setdiff", "setequal", "union"))
library(tidyr)
library(stringr)
library(ggplot2)

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
  inner_join(tbl(con, "variables") %>%
               filter(name %in% !!bety2try$bety_name),
             c("variable_id" = "id")) %>%
  select(scientificname, trait = name, value = mean) %>%
  collect()

bety_traits %>%
  left_join(select(pft_sp, pft, scientificname),
            by = "scientificname") %>%
  ggplot() +
  aes(x = scientificname, y = value, color = pft) +
  geom_jitter(size = 1, alpha = 0.5) +
  facet_wrap(vars(trait), scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90))

bety_traits %>%
  filter(grepl("Pinus", scientificname), trait == "SLA")

pinus_sla <- tbl(con, "traits") %>%
  # Pinus strobus; SLA
  filter(specie_id == 1017, variable_id == 15) %>%
  collect()

# All of the "bad" Pinus values  are from TRY!
pinus_sla %>%
  filter(mean > 10) %>%
  select(mean, id, notes)

bad_ids <- c(94162, 94450, 94459)

bad_data <- try_fst[try_fst$ObservationID %in% bad_ids,]
setDT(bad_data)
bad_data

try_data_ids <- try_fst[, c("DataName", "DataID")]
setDT(try_data_ids)

try_ids <- try_data_ids[, .N, .(DataName, DataID)]

try_ids[, DataID == 12]

try_pinus <- try_fst[try_fst$DataID %in% c(12, 6582) & try_fst$AccSpeciesName == "Pinus strobus", ]
setDT(try_pinus)

try_errors <- na.omit(try_fst[, "ErrorRisk"])
hist(try_errors[try_errors < 10])
quantile(try_errors, c(0.5, 0.9, 0.95, 0.975))

try_ids %>%
  .[grepl("SLA", DataName), ]


dat %>%
  as_tibble() %>%
  mutate(source = "TRY") %>%
  left_join(bety_traits, c(""))
