library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

future::plan(future.callr::callr())

out_root <- getOption("fortebaseline.ed_root")
ed_src <- getOption("fortebaseline.ed_src_dir")

ed_alt <- path(ed_src, "ED", "build", "ed_2.1-opt-crown-model-longwave-5c14e311")

run_edc <- partial(
  run_ed,
  start_date = "1902-06-01",
  end_date = "1950-01-02",
  ed_exe = ed_alt
)

fout <- partial(path, out_root)

l <- list(
  cts = run_edc("CTS"),
  fts = run_edc("FTS", crown_model = TRUE),
  cms = run_edc("CMS", multiple_scatter = TRUE),
  fms = run_edc("FMS", crown_model = TRUE, multiple_scatter = TRUE),
  ctp = run_edc("CTP", trait_plasticity = TRUE),
  ftp = run_edc("FTP", crown_model = TRUE, trait_plasticity = TRUE),
  cmp = run_edc("CMP", multiple_scatter = TRUE, trait_plasticity = TRUE),
  fmp = run_edc("FMP", crown_model = TRUE, multiple_scatter = TRUE,
                trait_plasticity = TRUE)
)

mods <- c("CTS", "FTS", "CMS", "FMS", "CTP", "FTP", "CMP", "FMP")
out_list <- path(out_root, mods) %>%
  map(read_efile_dir)

out_df <- bind_rows(out_list)

out_df %>%
  unnest(scalar) %>%
  ggplot() +
  aes(x = datetime, y = mmean_plresp_py) +
  geom_line() +
  facet_wrap(vars(basename))

year_avgs <- out_df %>%
  unnest(scalar) %>%
  mutate(year = lubridate::year(datetime)) %>%
  select(-c(case:datetime), -age, -outdir) %>%
  select_if(~!is.list(.x)) %>%
  select_if(~n_distinct(.x) > 1) %>%
  group_by(basename, year) %>%
  summarize_all(mean) %>%
  ungroup()

year_avgs %>%
  ## filter(year > 1915, year < 1945) %>%
  mutate(basename = fct_reorder(basename, mmean_npp_py, .desc = TRUE)) %>%
  ggplot() +
  aes(x = year, y = mmean_npp_py, color = basename) +
  geom_line() +
  scale_color_viridis_d()

year_avgs %>%
  filter(year > 1930) %>%
  select(-year) %>%
  group_by(basename) %>%
  summarize_all(list(m = mean, s = sd)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = basename, y = 10 * mmean_npp_py_m,
      ymin = 10 * (mmean_npp_py_m - mmean_npp_py_s),
      ymax = 10 * (mmean_npp_py_m + mmean_npp_py_s)) +
  geom_pointrange()


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
