library(fortebaseline)
library(here)
library(tidyverse)

options(
  fortebaseline.ed_root = fs::dir_create(here::here(
    "analysis", "data", "retrieved", "ed-tests"
  )),
  fortebaseline.ed_input_dir = fs::path_home(
    "projects", "pnnl", "forte-gcb", "forte_project",
    "umbs-ed-inputs"
  ),
  fortebaseline.ed_src_dir = fs::path_home(
    "projects", "models", "forte-ed2"
  )
)

# Median parameters
trait_distribution <- readRDS(here("analysis", "data",
                                   "retrieved", "trait-distribution.rds"))

medians <- trait_distribution %>%
  select(name = bety_name, trait, Median) %>%
  pivot_wider(id_cols = "name", names_from = "trait",
              values_from = "Median") %>%
  as_pft_list()

# Default parameters
default <- run_ed_maybe("default", end_date = "1921-01-01")
default$log()

# Try running a parameter
paramtest <- run_ed_maybe(
  "zz1",
  end_date = "1910-01-01",
  trait_values = list(umbs.early_hardwood = list(SLA = 13)),
  IQOUTPUT = 0,
  ISOUTPUT = 0,
  IADD_COHORT_MEANS = 1
)
paramtest$log()

# Try running a parameter
pt2 <- run_ed_maybe(
  "zz2",
  end_date = "1910-01-01",
  trait_values = list(umbs.early_hardwood = list(SLA = 15)),
  IQOUTPUT = 0,
  ISOUTPUT = 0,
  IADD_COHORT_MEANS = 1
)
pt2$log()

parameter <- "water_conductance"
pft <- "Early hardwood"

oaat_sensitivity <- function(parameter, pft,
                             trait_plasticity = FALSE,
                             multiple_scatter = FALSE,
                             crown_model = FALSE,
                             ...) {
  quants <- c(q025 = 0.025, q250 = 0.25,
              q750 = 0.75, q975 = 0.975)
  dat <- trait_distribution %>%
    dplyr::filter(trait == {{parameter}}, pft == {{pft}})
  stopifnot(nrow(dat) == 1)
  values <- rlang::exec(
    paste0("q", dat[["distn"]]),
    quants,
    dat[["parama"]],
    dat[["paramb"]]
  )
  prefix <- paste0(
    ifelse(crown_model, "F", "C"),
    ifelse(multiple_scatter, "M", "T"),
    ifelse(trait_plasticity, "P", "S")
  )
  outdir <- fs::dir_create(fs::path(here::here(
    "analysis", "data", "retrieved", "oaat-sensitivity",
    parameter, pft, prefix
  )))

  purrr::imap(values, function(trait, name) run_ed_maybe(
    casename = name,
    out_root = outdir,
    start_date = "1902-01-01",
    end_date = "1990-01-01",
    trait_plasticity = trait_plasticity,
    multiple_scatter = multiple_scatter,
    crown_model = crown_model,
    trait_values = rlang::list2(
      !!pfts("bety_name")[pfts("pft") == pft] := rlang::list2(
        !!parameter := trait
      )
    ),
    ISOUTPUT = 0
    ...
  ))

}

sensitivity_table <- trait_distribution %>%
  distinct(pft, trait) %>%
  expand_grid(
    trait_plasticity = c(TRUE, FALSE),
    multiple_scatter = c(TRUE, FALSE),
    crown_model = c(TRUE, FALSE)
  )

eh_wc_default <- oaat_sensitivity(
  "water_conductance",
  "Early hardwood",
  overwrite = TRUE
)

## writeLines(tail(readLines(eh_wc_default$q250$p$get_output_file()), 10))
results <- eh_wc_default %>%
  map("outdir") %>%
  map(fs::path, "monthly-output.rds") %>%
  map_dfr(readRDS)

results_pft <- results %>%
  select(basename, pft) %>%
  unnest(pft)

fracs <- results_pft %>%
  select(case, datetime, pft, agb_py) %>%
  mutate(pft = set_pft(pft)) %>%
  group_by(case, datetime) %>%
  mutate(frac = agb_py / sum(agb_py)) %>%
  ungroup() %>%
  select(-agb_py) %>%
  pivot_wider(names_from = pft, values_from = frac) %>%
  mutate(npft_eff = 1 / (`Early hardwood`^2 + `Mid hardwood`^2 + `Late hardwood`^2 + Pine^2))

results_pft

ggplot(results_pft) +
  aes(x = datetime, y = agb_py, color = factor(pft)) +
  geom_line() +
  facet_wrap(vars(case), scales = "fixed")

fracs %>%
  mutate(case = factor(case)) %>%
  ggplot() +
  aes(x = datetime, y = npft_eff, color = case) +
  geom_line()
