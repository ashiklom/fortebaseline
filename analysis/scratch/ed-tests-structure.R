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
