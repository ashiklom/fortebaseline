library(fortebaseline)
library(data.table)
library(magrittr)
library(ggplot2)

all_ivars <- c("case", "model_id", "param_id", "datetime")

scalar_ivars <- c()
## scalar_yvars <- c("mmean_nep_py", "mmean_npp_py", "mmean_rh_py",
##                   "mmean_plresp_py")
## scalar_yvars <- c("mmean_gpp_py", "mmean_npp_py", "mmean_plresp_py")
scalar_yvars <- c("mmean_available_water_py", "mmean_a_closed_py", "mmean_a_co2_py",
                  "mmean_a_decomp_py", "mmean_a_light_py", "mmean_a_net_py",
                  "mmean_")
scalar_vars <- c(all_ivars, scalar_ivars, scalar_yvars)
mscalar0 <- fst_scalar_monthly()
mscalar <- mscalar[, scalar_vars]
setDT(mscalar)

# Calculate annual means of fluxes
f_annual <- mscalar[, year := year(datetime)] %>%
  .[, lapply(.SD, mean), .(case, model_id, param_id, year),
    .SDcols = scalar_yvars]

# Calculate growing season means of LAI
pft_ivars <- c("pft")
pft_yvars <- c("mmean_lai_py")
pft_vars <- c(all_ivars, pft_ivars, pft_yvars)
mpft <- fst_pft_monthly() %>%
  .[, pft_vars]
setDT(mpft)

lai_annual <- mpft %>%
  .[, .(lai = sum(mmean_lai_py)), .(case, model_id, param_id, datetime)] %>%
  .[, year := year(datetime)] %>%
  .[month(datetime) %in% 6:8, ] %>%
  .[, .(lai = mean(lai)), .(case, model_id, param_id, year)]

setkey(f_annual, case, model_id, param_id, year)
setkey(lai_annual, case, model_id, param_id, year)
fl_annual <- f_annual[lai_annual]

fl_long <- melt(fl_annual, id.vars = c("case", "model_id", "param_id", "year"))
ggplot(fl_long) +
  aes(x = year, y = value, group = case) +
  geom_line(alpha = 0.1) +
  facet_grid(vars(variable), vars(model_id), scales = "free_y")

# Growing season means
mboth_gs_mean <- mboth %>%
  .[month(datetime) %in% 6:8, ][]

# Everything
dat_all <- mscalar[, scalar_vars]
setDT(dat_all)
dat_gs <- dat_all[month(datetime) %in% 6:8, ] %>%
  .[, year := year(datetime)] %>%
  .[, lapply(.SD, mean), .(case, model_id, param_id, year),
    .SDcols = scalar_yvars]

# All variables
cts <- mscalar[mscalar$model_id == "CTS",
               c("datetime", grep("mmean", names(mscalar), value = TRUE))]
setDT(cts)
cts_var <- vapply(cts, var, numeric(1))

cts_pca <- princomp(cts[, cts_var > 0, with = FALSE], cor = TRUE)

pcdat <- tibble::as_tibble(cts_pca$loadings[,1:3], rownames = "variable")

pcdat %>%
  dplyr::mutate(variable = forcats::fct_reorder(variable, Comp.2)) %>%
  ggplot() +
  aes(x = variable, y = Comp.2) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))


# Means for entire growing season

ix <- mscalar$datetime < "1931-01-01" & mscalar$datetime > "1915-01-01"
dat <- mscalar[ix, scalar_vars]
setDT(dat)

# Means of growing season values during this period
dat_m <- dat[month(datetime) %in% 6:8, ] %>%
  .[, year := year(datetime)] %>%
  .[, lapply(.SD, mean), .(case, model_id, param_id), .SDcols = scalar_yvars]

params <- fread(param_file)
params_wide <- params %>%
  melt(id.vars = c("param_id", "name")) %>%
  .[!is.na(value), ] %>%
  dcast(param_id ~ name + variable, sep = "..")

setkey(params_wide, param_id)
setkey(dat_m, param_id)
merged <- params_wide[dat_m]

param_cols <- names(params_wide)[-1]
rhs <- paste(param_cols, collapse = " + ")
form <- paste("mmean_gpp_py", rhs, sep = " ~ ")
fit <- lm(as.formula(form), data = merged[model_id == "CTS"])
fitc <- coefficients(fit)
important <- names(fitc[order(abs(fitc), decreasing = TRUE)])
step <- MASS::stepAIC(fit, direction = "backward")

stepsm <- summary(step)
stepcf <- coefficients(stepsm)
stepm <- stepcf[stepcf[, "Pr(>|t|)"] < 0.05, 1]
important <- stepm[order(abs(stepm), decreasing = TRUE)]
plot(step)

ggplot(merged[model_id == "CTS"]) +
  aes_string(x = important[6]) +
  aes(y = mmean_gpp_py) +
  geom_point() +
  geom_smooth(method = "lm")
