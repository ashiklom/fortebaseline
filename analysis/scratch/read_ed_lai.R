library(tidyverse)
library(data.table)
library(furrr)
library(fortebaseline)
library(cowplot)

plan(list(tweak(multiprocess, workers = 8)))

get_monthly_lai <- function(workflow_id, year,
                            startmonth = NULL) {
  if (is.null(startmonth)) {
    startmonth <- 1
    if (year == 1902) startmonth <- 6
  }

  nday <- as.numeric(difftime(
    sprintf("%d-01-01", year + 1),
    sprintf("%d-%d-01", year, startmonth),
    tz = "UTC",
    units = "days"
  ))
  nt <- nday * 24 * 2 # Half-hourly output

  filename <- glue::glue("analysis-T-{year}-00-00-000000-g01.h5")
  filepath <- pecanapi::run_dap(workflow_id, filename)

  nc <- ncdf4::nc_open(filepath)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  lai_raw <- ncdf4::ncvar_get(
    nc,
    "LAI_PY",
    start = c(6, 1, 1, 1),
    count = c(6, -1, -1, nt) # Only include PFTs 6, (7), 8, 9, 10, 11
  )[-2,,] # Drop PFT 7
  pft_levels <- c("Early hardwood", "Mid hardwood", "Late hardwood",
                  "Pine", "Late conifer")
  pftnames <- factor(pft_levels[c(4, 5, 1:3)], pft_levels)
  basedate <- ISOdate(year, startmonth, 1, 0, 0, 0, "UTC")

  data.table::melt(lai_raw) %>%
    tibble::as_tibble() %>%
    dplyr::rename(ipft = Var1, cohort = Var2, timestep = Var3) %>%
    dplyr::mutate(
      pft = pftnames[ipft],
      dt = basedate + lubridate::dhours((timestep - 1) / 2),
      month = lubridate::floor_date(dt, "month")
    ) %>%
    dplyr::group_by(month, pft, cohort) %>%
    dplyr::summarize(lai = mean(value)) %>%
    dplyr::summarize(lai = sum(lai)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(workflow_id = !!workflow_id)
}

get_ensemble_lai <- function(rundir, years = seq(1902, 1990)) {
  ensemble <- basename(rundir)
  future_map_dfr(years, get_monthly_lai, rundir = rundir) %>%
    dplyr::mutate(ensemble = !!ensemble)
}

runfile <- function(workflow_id, target, con = default_connection()) {
  runs <- pecanapi::list_runs(con, workflow_id)
  if (nrow(runs) == 0) return(NULL)
  pecanapi::output_url(workflow_id, file.path("run", runs[["id"]], target))
}

parse_ed2ins <- function(workflow_id, con = default_connection()) {
  filepath <- runfile(workflow_id, "ED2IN", con)
  PEcAn.ED2::read_ed2in(filepath)
}

try_numeric <- function(x) {
  tryCatch(as.numeric(x), error = function(e) x)
}

parse_configs <- function(workflow_id, con = default_connection()) {
  filepath <- runfile(workflow_id, "config.xml", con)
  conf_raw <- xml2::read_xml(filepath) %>%
    xml2::as_list()
  conf_raw[[1]] %>%
    setNames(c("Early hardwood", "Mid hardwood", "Late hardwood", "Pine")) %>%
    purrr::map_dfr(~tibble::tibble(!!!.x), .id = "pft") %>%
    dplyr::mutate_at(vars(-pft), purrr::simplify) %>%
    dplyr::mutate_at(vars(-pft), try_numeric)
}

con <- default_connection()

bety_workflows <- tbl(con, "workflows") %>%
  filter(start_date == "1902-06-01",
         end_date == "1912-12-31")

workflow_ids <- pull(bety_workflows, id)

workflows_years <- expand.grid(workflow_id = workflow_ids, year = seq(1902, 1912)) %>%
  as_tibble()
all_lai <- future_pmap(
  workflows_years,
  possibly(get_monthly_lai, NULL),
  .progress = TRUE
)

lai_data <- bind_rows(all_lai)

ed2in_dat <- tibble(workflow_id = workflow_ids) %>%
  mutate(ed2in = map(
    workflow_id, possibly(parse_ed2ins, NULL),
    con = default_connection()
  )) %>%
  filter(map_lgl(ed2in, negate(is.null)))

modify_if(list(1:5, 6, 7:10), ~length(.x) > 1, list)

ed2in_full <- ed2in_dat %>%
  mutate(ed2in = map(
    ed2in,
    ~as_tibble(modify_if(.x, ~length(.x) > 1, list))
  )) %>%
  unnest()

run_info <- ed2in_dat %>%
  mutate(
    rtm = map_dbl(ed2in, "ICANRAD"),
    crown_model = map_dbl(ed2in, "CROWN_MOD"),
    n_plant_lim = map_dbl(ed2in, "N_PLANT_LIM"),
    n_decomp_lim = map_dbl(ed2in, "N_DECOMP_LIM"),
    trait_plasticity = map_dbl(ed2in, "TRAIT_PLASTICITY_SCHEME")
    ) %>%
  select(-ed2in)

results <- lai_data %>%
  left_join(run_info) %>%
  group_by(month, workflow_id) %>%
  mutate(total_lai = sum(lai)) %>%
  ungroup(month, workflow_id)

fst::write_fst(results, "analysis/data/derived-data/ed-decade-lai-output.fst")

results_plot <- results %>%
  mutate(
    rtm = recode(rtm, `2` = "multiple scatter", `1` = "two-stream"),
    crown_model = recode(crown_model, `0` = "complete shading", `1` = "gap"),
    n_limitation = case_when(n_plant_lim & n_decomp_lim ~ "both",
                             as.logical(n_plant_lim) ~ "plant",
                             as.logical(n_decomp_lim) ~ "decomp",
                             TRUE ~ "none") %>%
      factor(c("both", "plant", "decomp", "none")) %>%
      fct_relabel(~paste("N limit:", .x)),
    trait_plasticity = recode(trait_plasticity, `0` = "static traits", `1` = "plastic traits")
  ) %>%
  ggplot() +
  aes(x = month, y = lai, color = pft) +
  geom_line() +
  facet_grid(rows = vars(crown_model, rtm),
             cols = vars(trait_plasticity, n_limitation)) +
  labs(y = "Leaf area index", color = "PFT") +
  scale_x_datetime(date_breaks = "2 years",
                   date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title.x = element_blank())
results_plot
save_plot("analysis/figures/lai_10year.png", results_plot,
          base_width = 10,
          base_height = 7)

##################################################

all_configs <- tibble(workflow_id = workflow_ids) %>%
  mutate(configs = map(
    workflow_id, possibly(parse_configs, NULL),
    con = default_connection()
  )) %>%
  filter(map_lgl(configs, negate(is.null))) %>%
  unnest()

configs_long <- all_configs %>%
  select(-num) %>%
  select_if(negate(is.list)) %>%
  gather(variable, value, -workflow_id, -pft)

configs_long %>%
  semi_join(
    configs_long %>%
      group_by(pft, variable) %>%
      summarize(variance = var(value)) %>%
      filter(variance != 0)
  ) %>%
  ggplot() +
  aes(x = pft, y = value) +
  geom_jitter() +
  facet_wrap(vars(variable), scales = "free_y")
