#' R implementation of ED2 two-stream radiation model
#'
#' @param czen Cosine of angle of incidence (`cosaio`)
#' @param iota_g Ground (soil + snow) albedo
#' @param pft PFT identities of each cohort, as integer (ncoh)
#' @param lai Leaf area index of each cohort (ncoh)
#' @param wai Wood area index of each cohort (ncoh)
#' @param cai Crown area of each cohort (ncoh)
#' @param orient_factor Orient factor (npft)
#' @param clumping_factor Clumping factor (npft)
#' @param leaf_reflect Leaf reflectance spectra (nwl * npft)
#' @param leaf_trans Leaf transmittance spectra (nwl * npft)
#' @param wood_reflect Wood reflectance spectra (nwl * npft)
#' @param wood_trans Wood transmittance spectra (nwl * npft)
#' @param down_sky Normalized diffuse solar spectrum (nwl)
#' @param down0_sky Normalized direct solar spectrum (nwl)
#' @param wavelengths Numeric vector of wavelengths to use, in nm
#'   (nwl). Default is 400:2500.
#' @return List of outputs
#' @author Alexey Shiklomanov
#' @export
two_stream <- function(ipft, L,
                       lr, lt, orient,
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

  # Unpack PFT variables
  orient <- orient[ipft]
  lr <- lr[ipft]
  lt <- lt[ipft]

  # Calculations from sfc_rad
  leaf_scatter <- lr + lt
  leaf_backscatter <- (leaf_scatter + 0.25 * (lr - lt) *
                         (1 + orient) ^ 2) / (2 * leaf_scatter)
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)
  mu_bar <- (1 - phi1 * log(1 + phi2 / phi1) / phi2) / phi2
  mu_bar[orient == 0] <- 1
  stopifnot(all(is.finite(mu_bar)))
  ##########

  # Size of solution matrix
  nsiz <- 2 * ncoh + 2

  # Loop over cohorts -- vectorizing here
  ## elai <- clumping_factor * lai
  elai <- L
  ## etai <- elai + wai
  etai <- elai

  ## leaf_weight <- elai / etai
  ## wood_weight <- 1 - leaf_weight

  cai <- rep(1, length(L))
  czen <- cos(theta * pi / 180)

  # Inverse optical depth of direct radiation
  proj_area <- phi1 + phi2 * czen
  mu0 <- -etai / log((1 - cai) + cai * exp(-proj_area * etai / (cai * czen)))

  # Inverse optical depth of diffuse radiation
  mu <- -etai / log((1 - cai) + cai * exp(-etai / mu_bar))

  # Backscatter coefficients for diffuse radiation
  iota_ratio <- 1 / (2 * (1 + phi2 * mu0)) *
    (1 - phi1 * mu0 / (1 + phi2 * mu0) *
       log((1 + (phi1 + phi2) * mu0) / (phi1 * mu0)))
  stopifnot(all(is.finite(iota_ratio)))
  beta0 <- iota_ratio * (1 + mu0 / mu)
  stopifnot(all(is.finite(beta0)))
  epsil0 <- 1 - 2 * beta0

  # Transmissivity of direct radiation
  expm0_minus <- exp(-etai / mu0)

  #################
  # Define boundary conditions
  #################
  z <- ncoh + 1
  elai[z] <- 0
  etai[z] <- 0
  ## leaf_weight[z] <- 0.5
  ## wood_weight[z] <- 0.5
  proj_area[z] <- 0.5
  mu0[z] <- czen / proj_area[z]
  mu[z] <- 1
  iota_ratio[z] <- 0.5 * (1 - 0.5 * mu0[z] *
                            log(1 / (0.5 * mu0[z]) + 1))
  beta0[z] <- iota_ratio[z] * (mu0[z] + mu[z]) / mu[z]
  epsil0[z] <- 1 - 2 * beta0[z]
  expm0_minus[z] <- 1

  # Direct radiation profile via exponential attentuation
  down0 <- numeric(z)
  down0[z] <- S0_beam
  for (j in seq(ncoh, 1)) {
    down0[j] <- down0[j + 1] * expm0_minus[j]
  }

  # Diffuse radiation property definitions
  # All of these are ({nwl x} ncoh)
  iota <- leaf_scatter
  ## iota <- leaf_weight[, -z] * leaf_scatter + wood_weight[, -z] * wood_scatter
  beta <- leaf_backscatter
  ## beta <- leaf_weight[, -z] * leaf_backscatter + wood_weight[, -z] * wood_backscatter
  epsil <- 1 - 2 * beta
  lambda <- sqrt((1 - epsil * iota) * (1 - iota)) / mu[-z]

  # Ancillary variables for right-hand side
  iota_mu <- iota / mu[-z]
  iota_mu0 <- iota / mu0[-z]
  down0_mu0 <- down0[-1] / mu0[-z]
  mu02 <- mu0[-z] ^ 2
  lambda2 <- lambda ^ 2

  a_aux <- -((1 - epsil * iota) * iota_mu + epsil0[-z] * iota_mu0) * down0_mu0
  s_aux <- -((1 - iota) * epsil0[-z] * iota_mu + iota_mu0) * down0_mu0
  delta <- (a_aux + s_aux) * mu02 / (2 * (1 - lambda2 * mu02))
  upsilon <- (a_aux - s_aux) * mu02 / (2 * (1 - lambda2 * mu02))

  # Upwelling and downwelling radiation
  iez <- sqrt((1 - iota) / (1 - epsil * iota))
  gamm_plus <- 0.5 * (1 + iez)
  gamm_minus <- 0.5 * (1 - iez)

  # Transmissivity of diffuse light
  expl_plus <- exp(lambda * etai[-z])
  expl_minus <- exp(-lambda * etai[-z])

  # Define boundary conditions for above canopy
  iota <- c(iota, 1)
  beta <- c(beta, 0)
  epsil <- c(epsil, 1 - 2 * beta[z])
  lambda <- c(lambda, 0)
  a_aux <- c(a_aux, -epsil0[z] * S0_beam / (mu0[z] ^ 2))
  s_aux <- c(s_aux, -iota[z] * S0_beam / (mu0[z] ^ 2))
  delta <- c(delta, 0.5 * (a_aux[z] + s_aux[z]) * mu0[z] ^ 2)
  upsilon <- c(upsilon, 0.5 * (a_aux[z] - s_aux[z]) * mu0[z] ^ 2)
  gamm_plus <- c(gamm_plus, 1)
  gamm_minus <- c(gamm_minus, 0)
  expl_plus <- c(expl_plus, 1)
  expl_minus <- c(expl_minus, 1)

  mmat <- matrix(0, nsiz, nsiz)
  yvec <- numeric(nsiz)

  # Bottom (1) and top boundary conditions
  mmat[1, 1] <- (gamm_minus[1] - alb_ground * gamm_plus[1]) * expl_minus[1]
  mmat[1, 2] <- (gamm_plus[1] - alb_ground * gamm_minus[1]) * expl_plus[1]
  mmat[nsiz, nsiz - 1] <- gamm_plus[z]
  mmat[nsiz, nsiz] <- gamm_minus[z]
  yvec[1] <- alb_ground * down0[1] - (upsilon[1] - alb_ground * delta[1]) * expm0_minus[1]
  yvec[nsiz] <- S0_diff - delta[z]

  for (k in seq_len(ncoh)) {
    kp1 <- k + 1
    k2 <- 2 * k
    k2m1 <- k2 - 1
    k2p1 <- k2 + 1
    k2p2 <- k2 + 2

    yvec[k2] <- delta[kp1] * expm0_minus[kp1] - delta[k]
    yvec[k2p1] <- upsilon[kp1] * expm0_minus[kp1] - upsilon[k]

    mmat[k2, k2m1] <- gamm_plus[k]
    mmat[k2, k2] <- gamm_minus[k]
    mmat[k2, k2p1] <- -gamm_plus[kp1] * expl_minus[kp1]
    mmat[k2, k2p2] <- -gamm_minus[kp1] * expl_plus[kp1]
    mmat[k2p1, k2m1] <- gamm_minus[k]
    mmat[k2p1, k2] <- gamm_plus[k]
    mmat[k2p1, k2p1] <- -gamm_minus[kp1] * expl_minus[kp1]
    mmat[k2p1, k2p2] <- -gamm_plus[kp1] * expl_plus[kp1]
  }

  stopifnot(is.finite(sum(mmat)))

  # Solve the radiation balance at each wavelength
  xvec <- solve(mmat, yvec)

  # Store the solution in matrices (nwl x (ncoh + 1))
  down <- numeric(ncoh + 1)
  up <- numeric(ncoh + 1)
  for (k in seq_len(ncoh + 1)) {
    k2 <- 2 * k
    k2m1 <- k2 - 1
    down[k] <- xvec[k2m1] * gamm_plus[k] * expl_minus[k] +
      xvec[k2] * gamm_minus[k] * expl_plus[k] +
      delta[k] * expm0_minus[k]
    up[k] <- xvec[k2m1] * gamm_minus[k] * expl_minus[k] +
      xvec[k2] * gamm_plus[k] * expl_plus[k] +
      upsilon[k] * expm0_minus[k]
  }

  ll_list <- integrate_light(down0, down, up)

  # Albedo is "up" at top of canopy
  c(
    list(
      albedo = up[z],                    # Albedo is upwelling radiation profile at top of canopy (nwl)
      up = up,                             # Upwelling radiation profile, by cohort + top of canopy (nwl x (ncoh + 1))
      down = down                         # Downwelling radiation, by cohort + top of canopy (nwl x (ncoh + 1))
    ),
    ll_list
  )
}
#' Average inverse optical depth for diffuse radiation: Two-stream
#'
#' With correction for finite crown area.
#'
#' @inheritParams two_stream
#' @return
#' @author Alexey Shiklomanov
#' @export
ts_mubar_star <- function(orient_factor, lai, cai = 1) {
  mubar <- mu_bar(orient_factor)
  -lai / log(1 - cai + cai * exp(-lai / mubar))
}

#' Transmissivity of diffuse radiation: Two-stream
#'
#' @inheritParams two_stream
#' @return
#' @author Alexey Shiklomanov
#' @export
ts_tau_diffuse <- function(lai, orient_factor, lr, lt, cai) {
  mu <- ts_mubar_star(orient_factor, lai, cai)
  iota <- scatter(lr, lt)
  beta <- backscatter(lr, lt, orient_factor)
  epsil <- 1 - 2 * beta
  lambda <- sqrt((1 - epsil * iota) * (1 - iota)) / mu
  exp(-lambda * lai)
}
