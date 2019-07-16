#' Transmissivity to diffuse radiation for the multiple-scatter model
#'
#' @inheritParams tau_direct 
#' @export
ms_tau_diffuse <- function(L, orient) {
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)
  ext_diff1 <- phi1 * L
  ext_diff2 <- phi2 * L
  -exp(-ext_diff1 - ext_diff2) *
    (ext_diff1^2 * exp(ext_diff1) * gsl::expint_Ei(-ext_diff1) + ext_diff1 - 1)
}

if (FALSE) {

  # Cohort variables
  # Order: bottom -> Top
  ipft <- c(2, 1, 1)
  L <- c(1, 1.5, 2)
  ncoh <- length(ipft)

  # PFT variables
  p4 <- rrtm::prospect4(1.4, c(40, 60), 0.01, 0.01)
  vis <- p4[(400:700) - 399, , ]
  lr <- colMeans(vis[, , 1])[ipft]
  lt <- colMeans(vis[, , 2])[ipft]
  orient <- c(0.2, 0.1)[ipft]

  # Site scalars
  theta <- 15
  S0_beam <- 0.8
  S0_diff <- 1 - S0_beam
  alb_ground <- 0.1

  # Common calculations
  nu <- lr + lt
  omega <- (nu + 0.25 * (lr - lt) * (1 - orient) ^ 2) / (2 * nu)
  phi1 <- phi1_f(orient)
  phi2 <- phi2_f(orient)

  # MS definitions
  # Note that these include boundary conditions for soil (i = 1) and
  # atmosphere (i = n + 2).
  a <- c(1 - alb_ground, (1 - nu)[ipft], 0)
  r <- c(1, omega[ipft], 0)
  t <- c(0, ms_tau_diffuse(L, orient), 1)
  tpsi <- c(0, tau_direct(L, theta, orient), 1)
  # This shouldn't need boundary conditions, so set them to NA to
  # make sure we don't accidentally try to use them.
  rpsi <- c(NA_real_, beta0(theta, orient), NA_real_)

  # Define some helpers (to simplify notation)
  ai <- 1 - a
  ri <- 1 - r
  ti <- 1 - t
  tpsii <- 1 - tpsi
  rpsii <- 1 - rpsi

  # Initialize vectors/matrices
  nsiz <- 2 * ncoh + 2
  M <- matrix(0, nsiz, nsiz)
  Y <- numeric(nsiz)
  S <- numeric(ncoh + 2)

  # Boundary conditions
  S[ncoh + 2] <- S0_beam
  M[1, 1] <- 1
  M[nsiz, nsiz] <- 1

  # Calculate direct radiation
  for (i in rev(seq_len(ncoh + 1))) {
    S[i] <- S[i + 1] * tpsi[i]
  }

  # NOTE: ED-2.2 defines Si as the radiation that reaches the BOTTOM
  # of layer i, but ZQ define it as radiation that reaches the TOP.
  # This means all of the S indices need be offset by 1.
  Y[1] <- S[2] * alb_ground

  # Set up the solver terms
  for (j in seq(1, ncoh)) {
    i <- j + 1
    im1 <- i - 1
    ip1 <- i + 1
    i2 <- 2 * j
    i2m1 <- i2 - 1
    i2p1 <- i2 + 1
    i2p2 <- i2 + 2
    Y[i2] <- S[ip1] * rpsi[i] *
      (1 - r[im1] * r[i] * ai[im1] * ti[im1] * ai[i] * ti[i]) *
      tpsii[i] * ai[i]
    Y[i2p1] <- S[ip1] * (1 - r[i] * r[ip1] * ai[i] * ti[i] * ai[ip1] * ti[ip1]) *
      tpsii[i] * ai[i] * rpsii[i]
    M[i2, i2m1] <- -(t[i] + ti[i] * ai[i] * ri[i])
    M[i2, i2] <- -r[im1] * (t[i] + ti[i] * ai[i] * ri[i]) * ai[im1] * ti[im1]
    M[i2, i2p1] <- 1 - r[im1] * r[i] * ai[im1] * ti[im1] * ai[i] * ti[i]
    M[i2p1, i2] <- 1 - r[i] * r[ip1] * ai[i] * ti[i] * ai[ip1] * ti[ip1]
    M[i2p1, i2p1] <- -r[ip1] * (t[i] + ti[i] * ai[i] * ri[i]) * ai[ip1] * ti[ip1]
    M[i2p1, i2p2] <- -(t[i] + ti[i] * ai[i] * ri[i])
  }

  # TODO: Not quite? Need to double check indices
  X <- solve(M, Y)


  
}
