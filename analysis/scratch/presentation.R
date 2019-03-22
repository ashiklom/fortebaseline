library(tidyverse)
library(fst)

## forte_dir <- "~/Box/Projects/forte_project/fortebaseline"
forte_dir <- "."

params_file <- file.path(
  forte_dir,
  "analysis/data/derived-data/ed-params.fst"
)
params <- read_fst(params_file) %>% as_tibble()

params_sub <- params %>%
  semi_join(
    params %>%
      group_by(pft, variable) %>%
      filter(sd(value) > 0) %>%
      ungroup() %>%
      distinct(variable)
  ) %>%
  rename(
    parameter = variable,
    parameter_value = value
  )

params_wide <- spread(params_sub, parameter, parameter_value)
params_ens <- params_sub %>%
  unite(pft_param, pft, parameter) %>%
  spread(pft_param, parameter_value)

lai_file <- file.path(
  forte_dir,
  "analysis/data/derived-data/ed-lai-output.fst"
)
lai <- read_fst(lai_file) %>% as_tibble()

lai_summary <- lai %>%
  group_by(year = lubridate::floor_date(month, "year"),
           pft, run_id = ensemble) %>%
  summarize(max_lai = max(lai)) %>%
  ungroup() %>%
  left_join(params_ens)

lai_maxes <- lai_summary %>%
  group_by_at(vars(-year, -max_lai)) %>%
  summarize(max_lai = max(max_lai))

trait_scatter <- function(pft, trait) {
  qtrait <- rlang::sym(paste(pft, trait, sep = "_"))
  lai_maxes %>%
    filter(pft != "Late conifer") %>%
    ggplot() +
    aes(x = !!qtrait, y = max_lai) +
    geom_point() +
    facet_grid(. ~ pft)
}
trait_scatter("Early hardwood", "nonlocal_dispersal")
trait_scatter("Pine", "q")

ggplot(params_sub) +
  aes(x = pft, y = parameter_value, fill = pft) +
  geom_violin() +
  geom_point(size = 0.5, alpha = 0.5) +
  facet_wrap(~parameter, scales = "free_y") +
  theme_bw() +
  theme(strip.text.x = element_text(size = rel(0.4)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("analysis/figures/parameters.png", width = 8, height = 4.5)


lai_summary %>%
  filter(pft != "Late conifer") %>%
  mutate(id = format(`Early hardwood_nonlocal_dispersal`)) %>%
  ggplot() +
  aes(x = year, y = max_lai, color = pft) +
  geom_line() +
  facet_wrap(~id)
