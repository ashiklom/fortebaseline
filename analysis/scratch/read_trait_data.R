import::from(magrittr, "%>%")
import::from(DBI, dbConnect)
import::from(RPostgres, Postgres)
import::from(dplyr, tbl, inner_join, select, filter,
             collect, pull)

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

writeLines(pft_species[["scientificname"]],
           file.path("analysis", "data", "derived-data", "species_list.txt"))

trait_data_sql <- pft_species_sql %>%
  inner_join(tbl(con, "traits"), by = "specie_id") %>%
  inner_join(tbl(con, "variables"),
             by = c("variable_id" = "id"),
             suffix = c(".value", ".variable"))
trait_data <- collect(trait_data_sql)

trait_data %>%
  filter(!is.na(mean)) %>%
  dplyr::count(notes.value, sort = TRUE) %>%
  print(n = 50)
 

library(ggplot2)

ggplot(trait_data) +
  aes(x = Symbol, y = mean) +
  geom_boxplot() +
  ## geom_jitter() +
  facet_wrap(~name.variable, scales = "free_y")

## con2 <- dbConnect(
##   RPostgreSQL::PostgreSQL(),
##   user = "bety",
##   password = "bety",
##   host = "localhost",
##   port = 7990
## )
## pftids <- unique(pft_species[["id.pft"]])
## purrr::map(pftids, PEcAn.DB::query.priors, con = con2)
