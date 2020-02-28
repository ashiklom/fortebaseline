#' Scale temperature dependent trait from measurement temperature to reference temperature
#'
#' @param observed.value observed value of temperature dependent trait, e.g. Vcmax, root respiration rate
#' @param old.temp temperature at which measurement was taken or previously scaled to
#' @param new.temp temperature to be scaled to, default = 25 C
#' @return numeric value at reference temperature
#' @export
arrhenius.scaling <- function(observed.value, old.temp, new.temp = 25) {
  new.temp.K <- udunits2::ud.convert(new.temp, "degC", "K")
  old.temp.K <- udunits2::ud.convert(old.temp, "degC", "K")
  return(observed.value / exp(3000 * (1 / (new.temp.K) - 1 / (old.temp.K))))
}
