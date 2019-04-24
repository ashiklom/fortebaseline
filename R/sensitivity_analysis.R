#' One-at-a-time PEcAn sensitivity analysis
#' 
#' NOTE: Fitting all at once results in too few degrees of freedom.
#' Need to fit one at a time.
#' @param y Response variable
#' @param x Dependent variable
#' @param alpha Fraction of median around which to calculate
#' derivative for sensitivity. Default = 0.01
#' @return `tibble` of sensitivity analysis results, with columns
#' `cv` (Coefficient of variation for `x`), `sensitivity`
#' (unnormalized sensitivity of `y` to `x` at median), `elasticity`
#' (normalized `sensitivity`), and `pvar` (prediction variance).
#' @export
sensitivity_analysis <- function(y, x, alpha = 0.01) {
  stopifnot(is.numeric(y), is.numeric(x))
  fit <- mgcv::gam(y ~ s(x))
  xmedian <- median(x)
  ymedian <- median(y)
  # CV -- Normalized parameter variance
  cv <- var(x) / xmedian
  # Sensitivity -- derivative at the median
  xpm <- xmedian * (1 + alpha * c(1, -1))
  predpm <- predict(fit, data.frame(x = xpm))
  sensitivity <- diff(predpm) / diff(xpm)
  # Elasticity -- normalized sensitivity
  elasticity <- sensitivity / (ymedian / xmedian)
  # Prediction variance
  pvar <- var(predict(fit))
  tibble::tibble(
    cv = cv,
    sensitivity = sensitivity,
    elasticity = elasticity,
    pvar = pvar
  )
}
