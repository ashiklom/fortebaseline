## library(fortebaseline)
library(tidyverse)
library(lubridate)
devtools::load_all(here::here(), attach_testthat = FALSE)
stopifnot(requireNamespace("fst", quietly = TRUE))

raw_monthly_output %>%
  select(dplyr::matches("agb"))

raw_monthly_output %>%
  filter(date == tail(date, 1), runs == unique(runs)[[3]]) %>%
  select(-(workflow_id:date))

  select(pft, nplant, nplant_py, mmean_lai_py, lai_co) %>%
  mutate(lai = lai_co * nplant,
         lai_sum = cumsum(lai))

f <- "analysis/data/derived-data/monthly-ensemble-output.fst"

raw_monthly_output %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything()) %>%
  left_join(workflow_structures(), by = "workflow_id")

result <- fst::read_fst(f) %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything()) %>%
  left_join(workflow_structures(), by = "workflow_id")

monthly_subset %>%
  mutate(
    year = floor_date(date, "years") %m+% lubridate::period(6, "months"),
    month = month(date)
  ) %>%
  filter(month %in% 7:9) %>%
  gather(variable, value, GPP:AGB) %>%
  mutate(variable = fct_inorder(variable)) %>%
  group_by(variable, year, add = TRUE) %>%
  summarize(
    mean = mean(value),
    lo = quantile(value, 0.2),
    hi = quantile(value, 0.8)
  ) %>%
  filter(!is.na(variable)) %>%
  mutate(model = interaction(crown, rtm, traits)) %>%
  ggplot() +
  aes(x = year, y = mean, ymin = lo, ymax = hi, fill = model, color = model) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_cowplot() +
  theme(axis.title.y = element_blank())
  

# LAI by PFT
result %>%
  group_by(workflow_id, crown, rtm, traits, date, pft) %>%
  summarize(lai_mean = mean(lai_co),
            lai_sd = sd(lai_co),
            lai_lo = quantile(lai_co, 0.25),
            lai_hi = quantile(lai_co, 0.75)) %>%
  ggplot() +
  aes(x = date, y = lai_mean, ymin = lai_lo, ymax = lai_hi) +
  geom_ribbon(aes(color = pft, fill = pft), alpha = 0.5) +
  ## geom_line(aes(color = pft)) +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)

# LAI by PFT
result %>%
  filter(date < "1905-01-01") %>%
  group_by(workflow_id, crown, rtm, traits, date, pft) %>%
  summarize(lai_mean = mean(lai_co),
            lai_sd = sd(lai_co),
            lai_lo = quantile(lai_co, 0.25),
            lai_hi = quantile(lai_co, 0.75)) %>%
  ggplot() +
  aes(x = date, y = lai_mean, ymin = lai_lo, ymax = lai_hi) +
  ## geom_ribbon(aes(color = pft, fill = pft), alpha = 0.5) +
  geom_line(aes(color = pft)) +
  facet_grid(vars(traits), vars(crown, rtm), labeller = label_both)

dat <- fst::read_fst(f) %>%
  as_tibble() %>%
  mutate(workflow_id = as.numeric(gsub("PEcAn_", "", workflows))) %>%
  select(workflow_id, everything())

dat %>%
  filter(date == min(date), runs == min(runs)) %>%
  ## select(starts_with("mmean")) %>%
  select_if(~n_distinct(.x) > 1) %>%
  glimpse()

monthly_means <- raw_monthly_output %>%
  group_by(workflow_id, crown, rtm, traits, date, runs, pft) %>%
  select(starts_with("mmean"), agb_py) %>%
  summarize_all(mean)

monthly_means %>%
  select(
    GPP = mmean_gpp_py,
    NPP = mmean_npp_py,
    LAI_pft = mmean_lai_py,
    AGB_pft = agb_py
  ) %>%
  summarize(
    GPP = mean(GPP),
    NPP = mean(NPP),
    LAI = sum(LAI_pft),
    AGB = sum(AGB_pft)
  )

monthly_means %>%
  filter(date > "1910-07-01", runs == unique(runs)[[1]]) %>%
  select(pft, dplyr::matches("lai"))


  summarize(
    total_lai = unique(mmean_lai_py),
    leaf_n = unique(mmean_bleaf_n_py), leaf_c = unique(mmean_bleaf_py),
    root_n = unique(mmean_broot_n_py), root_c = unique(mmean_broot_py),
    storage_n = unique(mmean_bstorage_n_py), storage_c = unique(mmean_bstorage_py)
  ) %>%
  select(-mmean_lai_py, -dplyr::matches("bleaf|broot|bstorage")) %>%
  summarize_all(mean) %>%
  rename_all(~gsub("^mmean_(.*)_py$", "\\1", .)) %>%
  left_join(workflow_structures(), by = "workflow_id")

plot_vars <- c("npp", "gpp", "rh", "total_lai")

# These give weird results (LAI > 20...?)
monthly_means %>%
  filter(total_lai > 20) %>%
  select(workflow_id, runs, date) %>%
  distinct(workflow_id, runs) %>%
  print(n = Inf)
  ## semi_join(dat, .)

monthly_means %>%
  filter(crown == "finite", rtm == "two-stream") %>%
  filter(date < "1906-01-01") %>%
  ggplot() +
  aes(x = date, y = carbon_st, color = traits, group = runs) +
  geom_line()

plt <- monthly_means %>%
  select(workflow_id, runs, date,
         crown, rtm, traits,
         !!plot_vars) %>%
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  filter(month %in% 6:8) %>%
  anti_join(filter(monthly_means, total_lai > 20), by = "runs") %>%
  group_by(workflow_id, runs, crown, rtm, traits, year) %>%
  summarize_at(plot_vars, mean) %>%
  gather(variable, value, !!plot_vars) %>%
  ggplot() +
  aes(x = factor(year), y = value,
      fill = interaction(crown, rtm, traits),
      color = interaction(crown, rtm, traits)) +
  ## geom_violin() +
  geom_boxplot() +
  ## geom_line() +
  ## geom_jitter() +
  facet_wrap(vars(variable), scales = "free_y") +
  labs(x = "Year") +
  theme_bw() +
  theme(legend.position = "bottom")
## dv <- imguR::imguR("png", width = 10, height = 8, units = "in", res = 300)
## plt
## imguR::imgur_off(dv)

#########################################
library(tidyverse)
library(fortebaseline)
library(lubridate, exclude = "here")
library(here)
library(fst)

outfile <- here("analysis", "data", "model_output", "cohort_output.fst")
raw_cohort <- fst(outfile)

lai <- raw_cohort[, c("workflow_id", "run_id", "datetime", "pft", "lai_co")] %>%
  as_tibble()

m_lai <- lai %>%
  filter(lubridate::month(datetime) %in% 6:8) %>%
  mutate(year = floor_date(datetime, "years")) %>%
  group_by(workflow_id, run_id, pft, year) %>%
  summarize(lai = mean(lai_co)) %>%
  ungroup() %>%
  mutate(pft = set_pft(pft))

gen_run_cluster <- function(dat, ...) {
  x <- dat %>%
    spread(run_id, lai, fill = 0)
  xm <- x %>%
    select(-pft, -year) %>%
    as.matrix() %>%
    t()
  kxm <- kmeans(xm, 4)
  tibble::enframe(kxm$cluster, "run_id", "group") %>%
    dplyr::mutate(run_id = as.numeric(run_id))
}

run_groups <- m_lai %>%
  group_by(workflow_id) %>%
  group_modify(gen_run_cluster)

use_runs <- run_groups %>%
  group_by(workflow_id, group) %>%
  slice(1)

m_lai %>%
  inner_join(use_runs, by = c("workflow_id", "run_id")) %>%
  ggplot() +
  aes(x = year, y = lai, color = pft) +
  geom_line() +
  facet_grid(vars(group), vars(workflow_id)) +
  labs(y = "Leaf area index", color = "PFT") +
  scale_color_manual(values = pfts("color")) +
  cowplot::theme_cowplot() +
  theme(strip.text.y = element_blank(),
        axis.title.x = element_blank())

#########################################

## lai %>%
##   group_by(workflow_id) %>%
##   summarize(
##     n = n_distinct(run_id),
##     datetime = max(datetime)
##   ) %>%
##   print(n = Inf)

## wide_lai <- m_lai %>%
##   spread(pft, lai, fill = 0)

## wide_lai_smry <- wide_lai %>%
##   filter(year > "1915-01-01") %>%
##   group_by(workflow_id, run_id) %>%
##   summarize_at(vars(-year), mean)

## classify_lai <- function(dat, ...) {
##   kxm <- kmeans(dat, 4)
##   dplyr::mutate(dat, group = kxm$cluster)
## }

## classify_lai <- function(dat, ...) {
##   pca <- princomp(dat)
##   scores <- pca[["scores"]]
##   dd <- dist(scores)
##   clust <- hclust(dd)
##   groups <- cutree(clust, k = 4)
##   dat %>%
##     dplyr::mutate(groups = groups)
## }

x <- m_lai %>%
  filter(workflow_id == 99000000144) %>%
  select(-workflow_id) %>%
  spread(run_id, lai, fill = 0)
clust <- kxm$cluster

## x <- as.matrix(iris[, -5])
## head(x)
## dim(x)
## dx <- dist(iris[, -5])
## hcl <- hclust(dx)
## plot(hcl)
## cutree(hcl, h = 4)
## km <- kmeans(x, 4)


## xmd <- as.matrix(dist(xm))
## lt <- lower.tri(xmd)
## xmd_long <- cbind(
##   which(lt, arr.ind = TRUE),
##   value = xmd[lt]
## ) %>%
##   as_tibble() %>%
##   arrange(desc(value))
## which(lower.tri(xmd), arr.ind = TRUE)
## xmd_long <- xmd[lower.tri(xmd)] %>%
##   reshape2::melt() %>%
##   as_tibble() %>%
##   arrange(desc(value))
## top2 <- rownames(which(xmd == max(xmd), arr.ind = TRUE))
## xmd2 <- xmd[-top2[1], -top2[1]]
## xmd[which.max(xmd)]
## which.max(xmd)

dat2 <- wide_lai %>%
  filter(workflow_id == 99000000144)

dat <- wide_lai_smry %>%
  group_by(workflow_id) %>%
  group_split() %>%
  .[[1]] %>%
  select(-workflow_id, -run_id)

run_groups <- wide_lai_smry %>%
  group_modify(classify_lai)

use_runs <- run_groups %>%
  group_by(workflow_id, groups) %>%
  slice(1) %>%
  select(workflow_id, run_id, groups)


dd <- dist(pca$scores)
clust <- hclust(dd)
groups <- cutree(clust, k = 5)
run_groups <- wide_lai_smry %>%
  select(workflow_id, run_id) %>%
  mutate(group = groups)

run_pca <- wide_lai_smry %>%
  bind_cols(as_tibble(pca$scores))

wide_lai2 <- wide_lai %>%
  bind_cols(as_tibble(pca$scores))

m_lai %>%
  group_by(workflow_id, year, run_id) %>%
  summarize(lai = sum(lai)) %>%
  summarize(mean = mean(lai),
            sd = sd(lai),
            lo = quantile(lai, 0.1),
            hi = quantile(lai, 0.9)) %>%
  ggplot() +
  aes(x = year, y = mean, ymin = lo, ymax = hi) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_line() +
  facet_grid(cols = vars(workflow_id))

m_lai_extreme <- m_lai %>%
  filter(pft == "Pine") %>%
  group_by(workflow_id, run_id) %>%
  summarize(lai = max(lai)) %>%
  mutate(rank = rank(lai))

closest_to <- function(x, to) x[which.min(abs(x - to))]

m_lai_extreme %>%
  filter(rank %in% c(max(rank), min(rank), quantile(rank, 0.25), median(rank), quantile(rank, 0.75)))

  filter(lai %in% c(max(lai), min(lai), quantile(lai, 0.25), median(lai), quantile(lai, 0.75))) %>%
  mutate(lai_rank = rank(lai)) %>%
  select(-lai)

m_lai %>%
  inner_join(m_lai_extreme, by = c("workflow_id", "run_id")) %>%
  ggplot() +
  aes(x = year, y = lai, color = pft) +
  geom_line() +
  facet_grid(vars(lai_rank), vars(workflow_id))

m_lai %>%
  group_by(workflow_id, year, pft) %>%
  sample_n(10)

ggplot() +
  aes(x = year, y = lai, color = pft) +
  geom_line()
