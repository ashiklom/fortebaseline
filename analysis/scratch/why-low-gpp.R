library(fortebaseline)
library(data.table)
library(magrittr)
library(ggplot2)

all_ivars <- c("case", "model_id", "param_id", "datetime")

scalar_ivars <- c()
## scalar_yvars <- c("mmean_nep_py", "mmean_npp_py", "mmean_rh_py",
##                   "mmean_plresp_py")
## scalar_yvars <- c("mmean_gpp_py", "mmean_npp_py", "mmean_plresp_py")
scalar_yvars <- c("mmean_gpp_py", "mmean_a_open_py", "mmean_a_closed_py", "mmean_a_co2_py",
                  "mmean_a_light_py", "mmean_a_net_py", "mmean_fsw_py", "mmean_fsn_py")
scalar_vars <- c(all_ivars, scalar_ivars, scalar_yvars)
mscalar0 <- fst_scalar_monthly()
ii <- mscalar0$datetime < "1910-01-01" & mscalar0$model_id == "CTS"
mscalar <- mscalar0[mscalar0$datetime < "1910-01-01", scalar_vars]
setDT(mscalar)

hist(mscalar$mmean_fsw_py)

mscalar %>%
  melt(id.vars = c("case", "model_id", "param_id", "datetime")) %>%
  ggplot() +
  aes(x = datetime, y = value, group = case) +
  geom_line(alpha = 0.1) +
  facet_wrap(vars(variable), scales = "free")
