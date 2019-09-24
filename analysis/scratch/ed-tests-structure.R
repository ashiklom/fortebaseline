library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

future::plan(future.callr::callr())

out_root <- getOption("fortebaseline.ed_root")

end <- "1910-01-01"

ed_alt <- path_home("Projects", "pecan_project", "ed2", "ED", "build",
                    "ed_2.1-opt-crown-model-longwave-5c14e311")

# Reference case
pdefault <- run_ed("default", end_date = end)

# Compare the different cases, under default parameters
pcrown <- run_ed("crown", end_date = end, crown_model = TRUE)
pms <- run_ed("ms", end_date = end, multiple_scatter = TRUE)
ptrait <- run_ed("trait", end_date = end, trait_plasticity = TRUE)
pcrown_alt <- run_ed("crown-alt", end_date = end, crown_model = TRUE,
                     ed_exe = ed_alt)

tail_ed_output("crown-alt")

ptrait$wait()

mdefault <- read_efile_dir(path(out_root, "default"))
mcrown <- read_efile_dir(path(out_root, "crown"))
mms <- read_efile_dir(path(out_root, "ms"))
mtrait <- read_efile_dir(path(out_root, "trait"))
mcrown_alt <- read_efile_dir(path(out_root, "crown-alt"))

mall <- bind_rows(mdefault, mcrown, mms, mtrait, mcrown_alt)

msub <- mall %>%
  filter(basename %in% c("default", "crown", "crown-alt"))

msub %>%
  unnest(scalar) %>%
  ggplot() +
  aes(x = datetime, y = mmean_gpp_py, color = basename) +
  geom_line()

msub %>%
  unnest(scalar) %>%
  pull(mmean_leaf_temp_py) %>%
  summary()

msub %>%
  unnest(pft) %>%
  ggplot() +
  aes(x = datetime, y = mmean_lai_py,
      color = basename) +
  geom_line() +
  facet_grid(vars(pft), scales = "free_y")

msub %>%
  select(-pft) %>%
  unnest(cohort) %>%
  ggplot() +
  aes(x = datetime, y = mmean_light_level_co, color = basename) +
  geom_line() +
  facet_grid(vars(pft))

msub2 <- mall %>%
  filter(basename %in% c("default", "ms"))

msub2 %>%
  select(-pft) %>%
  unnest(cohort) %>%
  filter(datetime > "1908-01-01") %>%
  ggplot() +
  aes(x = datetime, y = mmean_lai_co, color = basename) +
  geom_line() +
  facet_grid(vars(pft), scales = "free_y")


tfiles <- dir_ls(path(out_root, "tout"), glob = "*/analysis-T-*")
mfiles <- dir_ls(path(out_root, "tout"), glob = "*/analysis-E-*")

tnc <- ncdf4::nc_open(tfiles[[1]])
gpp <- ncdf4::ncvar_get(tnc, "FMEAN_GPP_PY")

sum(ncdf4::ncvar_get(tnc, "FMEAN_GPP_PY"))
sum(ncdf4::ncvar_get(tnc, "FMEAN_PLRESP_PY"))

mnc <- ncdf4::nc_open(mfiles[[1]])
ncdf4::ncvar_get(mnc, "MMEAN_GPP_PY") * 48 * 30
ncdf4::ncvar_get(mnc, "MMEAN_PLRESP_PY")

pdefault <- run_ed("default2", end_date = "1949-12-31")
pnowater <- run_ed("nowater", end_date = "1949-12-31", water_lim = FALSE)
pdefsoil <- run_ed("defsoil", end_date = "1949-12-31",
                   SLMSTR = rep(1.0, nrow(soil_data)))
pclay <- run_ed("clay", end_date = "1949-12-31",
                SLXCLAY = 0.68, SLXSAND = 0.20)

tail(readLines(path(out_root, "clay", "stdout.log")))

out_default <- read_efile_dir(path(out_root, "default2"))
out_nowater <- read_efile_dir(path(out_root, "nowater"))
out_defsoil <- read_efile_dir(path(out_root, "defsoil"))
out_clay <- read_efile_dir(path(out_root, "clay"))

scalar <- bind_rows(
  default = out_default$scalar,
  no_h2o_lim = out_nowater$scalar,
  slmstr = out_defsoil$scalar,
  clay = out_clay$scalar,
  .id = "config"
)

get_gs <- . %>%
  select(-case, -model_id, -param_id) %>%
  filter(lubridate::month(datetime) %in% 6:8) %>%
  mutate(year = lubridate::year(datetime)) %>%
  select(-datetime)

gs_mean <- scalar %>%
  get_gs %>%
  group_by(config, year) %>%
  summarize_all(mean) %>%
  ungroup()

dpft <- bind_rows(
  default = out_default$pft,
  no_h2o_lim = out_nowater$pft,
  slmstr = out_defsoil$pft,
  clay = out_clay$pft,
  .id = "config"
)

dpft_gs <- dpft %>%
  get_gs %>%
  group_by(config, year, pft) %>%
  summarize_all(mean) %>%
  ungroup()

dpft_gs %>%
  select(year, config, pft,
         basal_area_py, mmean_bleaf_py, mmean_broot_py, mmean_bstorage_py,
         mmean_lai_py, agb_py, nplant_py) %>%
  pivot_longer(-c(year, config, pft)) %>%
  ggplot() +
  aes(x = year, y = value, color = factor(pft), linetype = config) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  facet_wrap(vars(name), scales = "free_y")

lai <- dpft_gs %>%
  group_by(config, year) %>%
  summarize_at(
    vars(mmean_lai_py, nplant_py),
    sum
  ) %>%
  ungroup()

gs_mean %>%
  left_join(lai, c("config", "year")) %>%
  select(year, config,
         mmean_lai_py, nplant_py,
         starts_with("mmean_a_"),
         matches("[gn]pp"), matches("resp"), matches("_rh_")) %>%
  pivot_longer(-c(year, config)) %>%
  ggplot() +
  aes(x = year, y = value, color = config) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y")

ggplot(gs_mean) +
  aes(x = year, y = mmean_a_closed_py, color = config) +
  geom_line()

vcols <- out_default$scalar %>%
  summarize_all(n_distinct) %>%
  pivot_longer(everything()) %>%
  filter(
    value > 1,
    !(name %in% c("age", "mmean_qthroughfall_py", "mmean_throughfall_py"))
  ) %>%
  pull(name)

scalar %>%
  select(config, !!vcols) %>%
  pivot_longer(-c(datetime, config), "variable") %>%
  ggplot() +
  aes(x = datetime, y = value, color = config) +
  geom_line() +
  facet_wrap(vars(variable), scales = "free_y")
