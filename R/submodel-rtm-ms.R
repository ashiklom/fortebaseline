#' Transmissivity to diffuse radiation for the multiple-scatter model
#'
#' @param C Crown area index (CAI) (default = 1)
#' @inheritParams tau_direct
#' @export
ms_tau_diffuse <- function(L, orient, C = 1) {
  # Locally-exposed leaf area
  locetai <- L / C
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)
  ext_diff1 <- phi1 * locetai
  ext_diff2 <- phi2 * locetai
  (1 - C) -
    C * exp(-ext_diff1 - ext_diff2) *
    (ext_diff1 ^ 2 * exp(ext_diff1) *
       gsl::expint_Ei(-ext_diff1) + ext_diff1 - 1)
}

#' Multiple-scatter model
#'
#' Cohorts are arranged from bottom (`i = 1`) to top (`i = ncohort`)
#'
#' @param ipft Integer indices of PFTs (`icohort`)
#' @param lr Leaf reflectance (`ipft`)
#' @param lt Leaf transmittance (`ipft`)
#' @return List containin cohort-level downward beam radiation, and downward and
#'   upward diffuse radiation.
#' @inheritParams tau_direct
#' @author Alexey Shiklomanov
#' @export
multi_scatter <- function(ipft, L,
                          # PFT-level parameters
                          lr, lt, orient,
                          # Site scalars
                          theta = 15,
                          S0_beam = 0.8,
                          S0_diff = 1 - S0_beam,
                          alb_ground = 0.1) {

  # Cohort variables
  # Order: bottom -> Top
  ncoh <- length(ipft)
  npft <- length(lr)
  stopifnot(
    length(L) == ncoh,
    length(lt) == npft,
    length(orient) == npft,
    max(ipft) <= npft
  )

  # Convert to PFT variables
  orient <- orient[ipft]
  lr <- lr[ipft]
  lt <- lt[ipft]

  # Common calculations
  nu <- lr + lt
  omega <- (nu + 0.25 * (lr - lt) * (1 - orient) ^ 2) / (2 * nu)
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)

  # Multi-scatter definitions
  # Soil -> n = 0
  # Atmosphere -> n = ncoh + 1
  # Absorbance (alpha in ED2)
  a <- fvector(seq(0, ncoh + 1))
  a[0] <- 1 - alb_ground
  a[ncoh + 1] <- 0
  a[1:ncoh] <- (1 - nu)[ipft]
  ai <- 1 - a  # `oma` in ED2

  # Diffuse reflectance (r_diff in ED2)
  r <- fvector(seq(0, ncoh + 1))
  r[0] <- 1
  r[ncoh + 1] <- 0
  r[1:ncoh] <- omega[ipft]
  ri <- 1 - r  # omr_diff

  # Diffuse transmittance (tau_diff in ED2)
  t <- fvector(seq(0, ncoh + 1))
  t[0] <- 0
  t[ncoh + 1] <- 1
  t[1:ncoh] <- ms_tau_diffuse(L, orient)
  ti <- 1 - t   # omt_diff

  # Direct transmittance (tau_beam in ED2)
  tpsi <- fvector(seq(0, ncoh + 1))
  tpsi[0] <- 0
  tpsi[ncoh + 1] <- 1
  tpsi[1:ncoh] <- tau_direct(L, theta, orient)
  tpsii <- 1 - tpsi  # omt_beam

  # Direct reflectance (r_beam in ED2)
  # This shouldn't need boundary conditions, so set them to NA to
  # make sure we don't accidentally try to use them.
  rpsi <- fvector(seq(0, ncoh + 1))
  rpsi[0] <- NA_real_
  rpsi[ncoh + 1] <- NA_real_
  rpsi[1:ncoh] <- beta0(theta, orient)
  rpsii <- 1 - rpsi  # omr_beam

  # Initialize vectors/matrices
  nsiz <- 2 * ncoh + 2
  # `amat` in ED2
  M <- matrix(0, nsiz, nsiz)
  # `cvec` in ED2
  Y <- numeric(nsiz)

  # Downward beam radiation (`beam_down` in ED2)
  S <- numeric(ncoh + 1)
  # Downward and upward diffuse radiation (synonyms)
  swd0 <- fvector(seq(ncoh + 1))
  swu0 <- fvector(seq(0, ncoh))

  # NOTE: ED-2.2 defines Si as the radiation that reaches the BOTTOM
  # of layer i, but ZQ define it as radiation that reaches the TOP.

  # Radiation at top of atmosphere is an input
  S[ncoh + 1] <- S0_beam
  swd0[ncoh + 1] <- S0_diff

  # The remaining radiation decays exponentially
  for (i in rev(seq_len(ncoh))) {
    S[i] <- S[i + 1] * tpsi[i]
  }

  # Matrix upper left and bottom right corners are 1
  M[1, 1] <- 1
  M[nsiz, nsiz] <- 1
  # At soil, upward flux is downward flux x soil albedo
  Y[1] <- S[1] * alb_ground
  # At atmospheric boundary, downward flux is an input
  Y[nsiz] <- S0_diff

  # Set up the solver terms
  for (i in seq(1, ncoh)) {
    im1 <- i - 1
    ip1 <- i + 1
    i2 <- 2 * i
    i2m1 <- i2 - 1
    i2p1 <- i2 + 1
    i2p2 <- i2 + 2
    # Vector
    Y[i2] <- (1 -
                r[im1] * ai[im1] * ti[im1] *
                r[i] * ai[i] * ti[i]) *
      ai[i] * tpsii[i] * rpsi[i] * S[ip1]
    Y[i2] <- (1 -
                r[i] * ai[i] * ti[i] *
                r[ip1] * ai[ip1] * ti[ip1]) *
      ai[i] * tpsii[i] * rpsii[i] * S[ip1]
    # Matrix
    M[i2, i2m1] <- -(t[i] + ti[i] * ai[i] * ri[i])
    M[i2, i2] <- -r[im1] * ai[im1] * ti[im1] * (t[i] + ti[i] * ai[i] * ri[i])
    M[i2, i2p1] <- 1 - ri[im1] * ai[im1] * ti[im1] *
      r[i] * ai[i]
    M[i2, i2p1] <- 1 - r[im1] * ai[im1] * ti[im1] * r[i] * ai[i] * ti[i]
    M[i2p1, i2] <- 1 - r[i] * ai[i] * ti[i] * r[ip1] * ai[ip1] * ti[ip1]
    M[i2p1, i2p1] <- -r[ip1] * ai[ip1] * ti[ip1] *
      (t[i] + ti[i] * ai[i] * ri[i])
    M[i2p1, i2p2] <- -(t[i] + ti[i] * ai[i] * ri[i])
  }

  X <- solve(M, Y)

  # Downward and upward diffuse fluxes, without scattering
  swu0[0] <- X[1]
  swd0[ncoh + 1] <- X[nsiz]
  swd0[1:ncoh] <- X[2 * (1:ncoh)]
  swu0[1:ncoh] <- X[2 * (1:ncoh) + 1]

  # Shortwave diffuse flux, with scattering:
  # Downard...
  i <- seq(1, ncoh + 1)
  im1 <- i - 1
  swd <- (swd0 + ri[i] * ai[i] * ti[i] * swu0[im1]) /
    (1 - r[im1] * ai[im1] * ti[im1] * r[i] * ai[i] * ti[i])

  # ...and upward
  swu <- fvector(seq(0, ncoh))
  i <- seq(0, ncoh)
  ip1 <- i + 1
  swu <- (swu0 + r[i] * ai[i] * ti[i] * swd0[ip1]) /
    (1 - r[i] * ai[i] * ti[i] * r[ip1] * ai[ip1] * ti[ip1])

  swd2 <- numeric(length(swd))
  swd2[] <- swd
  swu2 <- numeric(length(swu))
  swu2[] <- swu

  ## list(
  ##   albedo = swu[ncoh],
  ##   diff_down = swd2,
  ##   diff_up = swu2
  ## )

  ll_list <- integrate_light(S, swd2, swu2)

  c(list(
    albedo = swu[ncoh],
    beam_down = S,
    diff_down = swd2,
    diff_up = swu2
  ), ll_list)

}
