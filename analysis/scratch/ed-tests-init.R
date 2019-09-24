library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

future::plan(future.callr::callr())

out_root <- getOption("fortebaseline.ed_root")
in_root <- getOption("fortebaseline.ed_input_dir")

end <- "1910-01-01"

ed_alt <- path_home("Projects", "pecan_project", "ed2", "ED", "build",
                    "ed_2.1-opt-crown-model-longwave-5c14e311")

soilplot <- function(dat) {
  dat %>%
    unnest(soil) %>%
    select(-c(basename:param_id), -outdir, -pft,
           -starts_with("dmean")) %>%
    pivot_longer(-c(datetime, slz)) %>%
    ggplot() +
    aes(x = datetime, y = value, color = factor(slz)) +
    geom_line() +
    facet_wrap(vars(name), scales = "free_y")
}
atmplot <- function(dat) {
  dat %>%
    unnest(scalar) %>%
    select(datetime, matches("mmean_atm_")) %>%
    pivot_longer(-datetime) %>%
    ggplot() +
    aes(x = datetime, y = value) +
    geom_line() +
    facet_wrap(vars(name), scales = "free_y")
}
pftplot <- function(dat) {
  dat %>%
    unnest(pft) %>%
    select(-c(basename:param_id), -outdir) %>%
    pivot_longer(-c(datetime, pft)) %>%
    mutate(pft = factor(pft, c(9:11, 6), labels = c("Early", "Mid", "Late", "Pine"))) %>%
    ggplot() +
    aes(x = datetime, y = value, color = pft) +
    geom_line() +
    facet_wrap(vars(name), scales = "free_y")
}
fluxplot <- function(dat) {
  dat %>%
    unnest(scalar) %>%
    select(datetime, matches("mmean_a_"), mmean_npp_py, mmean_gpp_py,
           mmean_rh_py, mmean_plresp_py) %>%
    pivot_longer(-datetime) %>%
    ggplot() +
    aes(x = datetime, y = value) +
    geom_line() +
    facet_wrap(vars(name), scales = "free_y")
}

bh_veg <- PEcAn.ED2::read_ed_veg("~/laptop_folders/Projects/nasa-rtm/edr-da/sites/BH07_site_1-25669/FFT.2008.")
bh_veg$latitude <- 45.5625
bh_veg$longitude <- -84.6975
bh_veg$css$time <- 1901
bh_veg$pss$time <- 1901

newbh_prefix <- dir_create(path("analysis", "data", "retrieved", "bh-input", "bhtest"))
newbh <- PEcAn.ED2::write_ed_veg(bh_veg, newbh_prefix)

ed_bh <- run_ed("bhtest", end_date = "1910-01-01",
                SFILIN = newbh_prefix,
                IED_INIT_MODE = 3)

ed_bh_out <- read_efile_dir(path(out_root, "bhtest"))

ed_bh_out %>%
  unnest(scalar) %>%
  select(-c(basename:param_id),
         -cohort, -pft, -soil, -outdir,
         -latitude, -longitude,
         -starts_with("mmsqu"),
         -c(ncohorts_global:yatm),
         -c(age:area_si), -c(dist_type:lsl),
         -matches("storage_resp"), -matches("drainage")) %>%
  filter(datetime < "1906-01-01", datetime > "1904-01-01") %>%
  ## select(datetime, starts_with("mmean_a_"),
  ##        mmean_plresp_py, mmean_rh_py, mmean_npp_py) %>%
  select(datetime, matches("vpdef"), matches("atm")) %>%
  pivot_longer(-datetime) %>%
  ggplot() +
  aes(x = datetime, y = value) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y")

ed_bh_out %>%
  select(-pft) %>%
  unnest(cohort) %>%
  select(datetime, pft, starts_with("mmean_a_"),
         mmean_fsw_co) %>%
  filter(datetime < "1904-01-01") %>%
  pivot_longer(-c(datetime, pft)) %>%
  ggplot() +
  aes(x = datetime, y = value, color = factor(pft)) +
  geom_point() +
  facet_wrap(vars(name), scales = "free_y")

bh2 <- run_ed("bh2", end_date = "1905-01-01",
              SFILIN = newbh_prefix,
              IED_INIT_MODE = 3,
              STGOFF = rep(0, 7))
bh2$wait()
bh2_out <- read_efile_dir(path(out_root, "bh2"))

bh2_out %>%
  unnest(scalar) %>%
  select(datetime, starts_with("mmean_a_"),
         mmean_npp_py, mmean_leaf_temp_py,
         mmean_atm_temp_py, mmean_can_temp_py) %>%
  pivot_longer(-datetime) %>%
  ggplot() +
  aes(x = datetime, y = value) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y")

soilplot(bh2_out)

max_sm <- run_ed("max-sm", end_date = "1904-01-01",
                 SFILIN = newbh_prefix,
                 IED_INIT_MODE = 3,
                 SLMSTR = rep(2.0, 7))
max_sm$wait()
maxsm_out <- read_efile_dir(path(out_root, "max-sm"))

maxsm_out %>%
  unnest(scalar) %>%
  ggplot() +
  aes(x = datetime, y = mmean_qrunoff_py) +
  geom_line()

bhdef_p <- run_ed("bh-default-soil", end_date = "1905-01-01",
                  SFILIN = newbh_prefix,
                  IED_INIT_MODE = 3,
                  SLZ = c(-2.307, -1.789, -1.340, -0.961, -0.648,
                          -0.4, -0.215, -0.089, -0.02),
                  SLMSTR = rep(1.0, 9),
                  STGOFF = rep(0, 9),
                  NZG = 9)

default <- read_efile_dir(path(out_root, "default"))

default %>%
  unnest(scalar) %>%
  filter(datetime > "1903-01-01") %>%
  ggplot() +
  aes(x = datetime, y = mmean_atm_temp_py) +
  geom_line()

default %>%
  unnest(soil) %>%
  select(-c(basename:param_id), -outdir, -pft,
         -starts_with("dmean")) %>%
  pivot_longer(-c(datetime, slz)) %>%
  ggplot() +
  aes(x = datetime, y = value, color = factor(slz)) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y")

dir_delete(path(out_root, "messing"))
ptest <- run_ed("messing",
                ed_exe = ed_alt,
                start_date = "1903-01-01",
                end_date = "1905-01-01",
                SFILIN = newbh_prefix,
                ED_MET_DRIVER_DB = path(in_root, "met",
                                        "CRUNCEP_ED2_site_1-33",
                                        "ED_MET_DRIVER_HEADER"),
                IED_INIT_MODE = 3)
ptest$wait()
## tail_ed_output("messing")
dtest <- read_efile_dir(path(out_root, "messing"), save = FALSE)

atmplot(dtest)
fluxplot(dtest)
soilplot(dtest)
