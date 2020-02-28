#' ED2 default parameters
#'
#' @return Tidy `data.frame` of ED2 default parameter values
#' @author Alexey Shiklomanov
#' @export
ed_default_params <- function() {
  f <- system.file("history.rgit.csv", package = "fortebaseline")
  stopifnot(file.exists(f))
  read.table(f, sep = ";", as.is = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      # Invert conversions from `PEcAn.ED2::convert.samples.ED`
      fineroot2leaf = q,
      Vcmax = arrhenius.scaling(Vm0, 15, 25),
      leaf_respiration_rate_m2 = arrhenius.scaling(Rd0, 15, 25),
      # 2 here is 1 / default maintenance respiration
      root_respiration_rate = 2 * arrhenius.scaling(
        root_respiration_factor, 15, 25
      ),
      # 0.48 is default leaf C
      SLA = SLA * 0.48
    ) %>%
    dplyr::filter(num %in% c(6, 9:11)) %>%
    tidyr::pivot_longer(
      -num,
      names_to = "trait",
      values_to = "default_value"
    ) %>%
    dplyr::inner_join(pfts(), "num")
}
