#' Leaf orientation coefficients
#'
#' @param orient Leaf orientation factor (-1, 1)
#' @export
phi1_f <- function(orient) {
  0.5 - 0.633 * orient - 0.33 * orient ^ 2
}

#' @rdname phi1_f
#' @export
phi2_f <- function(orient, phi1 = phi1_f(orient)) {
  0.877 * (1 - 2 * phi1)
}

#' Relative (G) or absolute (K) projected leaf area in the target direction
#'
#' @param theta Projection angle (degrees)
#' @param orient Leaf orientation factor (-1, 1)
#' @param theta_rad Projection angle (radians)
#' @export
gfunction <- function(theta, orient, theta_rad = theta * pi / 180) {
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)
  mu <- cos(theta_rad)
  phi1 + phi2 * mu
}

#' @rdname gfunction
#' @export
kfunction <- function(theta, orient, theta_rad = theta * pi / 180) {
  gfunction(theta, orient, theta_rad) / cos(theta_rad)
}

#' Average inverse optical depth
#'
#' @inheritParams gfunction
#' @export
mu_bar <- function(orient) {
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)
  x <- 1 - phi1 / phi2 * log(1 + phi2 / phi1)
  out <- x / phi2
  out[orient == 0] <- 1
  out
}

#' Beam backscatter
#'
#' @inheritParams gfunction
#' @export
beta0 <- function(theta, orient) {
  K <- kfunction(theta, orient)
  mubar <- mu_bar(orient)
  single_scatter_albedo(theta, orient) * (1 + mubar * K) / (mubar * K)
}

#' Single-scattering albedo
#'
#' @inheritParams gfunction
#' @export
single_scatter_albedo <- function(theta, orient, theta_rad = theta * pi / 180) {
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)
  G <- gfunction(theta, orient)
  cos_theta <- cos(theta_rad)
  0.5 * G / (cos(theta_rad) * phi2 + G) *
    (1 - cos_theta * phi1 / (cos_theta + G) * log((cos_theta * phi1 + cos_theta * phi2 + G) / (cos_theta * phi1)))
}

#' Transmissivity to direct radiation
#'
#' @param L Leaf (or total) area index
#' @inheritParams kfunction
#' @author Alexey Shiklomanov
#' @export
tau_direct <- function(L, theta, orient) {
  K <- kfunction(theta, orient)
  exp(-K * L)
}

#' Integrate light levels from flux estimates
#'
#' @param down0 Cohort-level direct downwelling flux
#' @param down Cohort-level diffuse downwelling flux
#' @param up Cohort-level diffuse upwelling flux
#' @return List containing overall light level, and levels of direct and diffuse light
#' @author Alexey Shiklomanov
#' @export
integrate_light <- function(down0, down, up) {
  # Integrate light levels
  ncoh <- length(down0) - 1
  light_level <- numeric(ncoh)
  light_beam_level <- numeric(ncoh)
  light_diff_level <- numeric(ncoh)

  for (k in seq_len(ncoh)) {
    kp1 <- k + 1
    light_level[k] <- light_level[k] + 0.5 * (down[k] + down[kp1] + down0[k] + down0[kp1])
    light_beam_level[k] <- light_beam_level[k] + 0.5 * (down0[k] + down0[kp1])
    light_diff_level[k] <- light_diff_level[k] + 0.5 * (down[k] + down[kp1])
  }

  list(
    light_level = light_level,
    light_beam_level = light_beam_level,
    light_diff_level = light_diff_level
  )
}
