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
  end_date = "1920-01-02",
  ed_exe = ed_alt
)

fout <- partial(path, out_root)

defparam <- ed_default_params()

params <- drake::readd(trait_distribution) %>%
  select(pft, trait, Mean, Median, lo, hi) %>%
  left_join(select(defparam, pft, trait, default_value),
            c("pft", "trait"))

params %>%
  mutate(reldiff = (Mean - default_value) / default_value) %>%
  arrange(desc(abs(reldiff))) %>%
  print(n = 40)

params %>%
  distinct(trait) %>%
  pull()

# Looks like growth respiration has the largest difference. Start with that one.
grvals <- seq(0.05, 0.5, 0.05)
gr_out <- imap(
  grvals,
  ~run_ed(paste0("gr", .y),
          trait_values = list(umbs.early_hardwood =
                                list(growth_resp_factor = .x)),
          end_date = "1910-01-01")
)

grdat <- map_dfr(
  seq_along(gr_out),
  ~read_efile_dir(path(out_root, paste0("gr", .x)))
)

out_long <- read_efile_dir(path(out_root, "default-long"))
out2 <- read_efile_dir(path(out_root, "default2"))

out_long %>%
  unnest(scalar) %>%
  ggplot() +
  aes(x = datetime, y = udunits2::ud.convert(mmean_gpp_py, "kg m-2", "g m-2")) +
  geom_line()

out_long %>%
  unnest(pft) %>%
  ggplot() +
  aes(x = datetime, y = basal_area_py) +
  geom_line() +
  facet_grid(vars(pft))

dat_def <- read_efile_dir(path(out_root, "default"))

dat_def %>%
  unnest(scalar) %>%
  ggplot() +
  aes(x = datetime, y = mmean_gpp_py) +
  geom_line()

grd <- grdat %>%
  bind_rows(dat_def) %>%
  mutate(growth_resp = c(grvals, 0))

grd %>%
  unnest(scalar) %>%
  ggplot() +
  aes(x = datetime, y = mmean_gpp_py, group = basename, color = growth_resp) +
  geom_line() +
  scale_color_viridis_c()

grd %>%
  unnest(pft) %>%
  ggplot() +
  aes(x = datetime, y = mmean_lai_py, color = growth_resp, group = basename) +
  geom_line() +
  facet_grid(vars(pft)) +
  scale_color_viridis_c()
