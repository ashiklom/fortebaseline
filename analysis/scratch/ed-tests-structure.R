library(fortebaseline)
library(fs)
library(here)
library(tidyverse)

future::plan(future.callr::callr())

out_root <- getOption("fortebaseline.ed_root")
ed_src <- getOption("fortebaseline.ed_src_dir")
ed_alt <- getOption("fortebaseline.ed_alt")

stopifnot(dir_exists(out_root), dir_exists(ed_src), file_exists(ed_alt))

structure_runs <- expand_grid(
  trait_plasticity = c(TRUE, FALSE),
  multiple_scatter = c(TRUE, FALSE),
  crown_model = c(TRUE, FALSE)
) %>%
  mutate(casename = paste0("default-",
                           if_else(crown_model, "F", "C"),
                           if_else(multiple_scatter, "M", "T"),
                           if_else(trait_plasticity, "P", "S")),
         start_date = "1902-01-01",
         end_date = "1999-12-31",
         ed_exe = ed_alt)

structure_raw <- pmap(structure_runs, run_ed_maybe)

outdir <- path(out_root, "default-CTS")

structure_results <- structure_raw %>%
  map("outdir") %>%
  map_dfr(read_efile_dir)

structure_results %>%
  rename(casename = basename) %>%
  right_join(structure_runs, "casename") %>%
  select(casename, trait_plasticity, multiple_scatter, crown_model,
         scalar, cohort, soil, pft_py = pft, outdir) %>%
  saveRDS(here("analysis", "data", "retrieved", "structure-default.rds"))

structure_results %>%
  select(casename = basename, scalar) %>%
  unnest(scalar) %>%
  select(-c(case:param_id))


structure_runs$out

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

figdir <- dir_create("analysis", "figures", "fresh")

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
  mutate(basename = factor(basename, mods)) %>%
  ggplot() +
  aes(x = year, y = 10 * mmean_npp_py, color = basename) +
  geom_line() +
  geom_hline(yintercept = 6.5, linetype = "dashed") +
  geom_hline(yintercept = c(6, 7), linetype = "dotted") +
  scale_color_brewer(palette = "Paired") +
  ylab(expression("NPP" ~ (MgC ~ ha ^ {-1})))
  theme_bw()

ggsave(path(figdir, "npp-structure-ts.png"))

year_avgs %>%
  filter(year > 1930) %>%
  select(-year) %>%
  group_by(basename) %>%
  summarize_all(list(m = mean, s = sd)) %>%
  ungroup() %>%
  mutate(basename = factor(basename, mods)) %>%
  ggplot() +
  aes(x = basename, y = 10 * mmean_npp_py_m,
      ymin = 10 * (mmean_npp_py_m - mmean_npp_py_s),
      ymax = 10 * (mmean_npp_py_m + mmean_npp_py_s)) +
  geom_pointrange() +
  geom_hline(yintercept = 6.5, linetype = "dashed") +
  geom_hline(yintercept = c(6, 7), linetype = "dotted") +
  xlab("Model configuration") +
  ylab("NPP (Mean +/- 1 SD) (MgC ha-1)") +
  theme_bw()

ggsave(path(figdir, "npp-mean-structure.png"))
