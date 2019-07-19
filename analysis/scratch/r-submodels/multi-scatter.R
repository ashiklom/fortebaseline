# devtools::install_github("r-lib/rray")
library(rray)
stopifnot(requireNamespace("gsl", quietly = TRUE))

random_leaf_angle <- function(theta, orient) {
  phi1 <- 0.5 - 0.633 * orient - 0.33 * orient ^ 2
  phi2 <- 0.877 * (1 - 2 * phi1)
  mu <- cos(theta)
  (phi1 + phi2 * mu) / mu
}

curve(random_leaf_angle(x * pi / 180, 0), 0, 89)
curve(random_leaf_angle(x * pi / 180, -0.2), 0, 89, add = TRUE, col = "cyan")
curve(random_leaf_angle(x * pi / 180, -0.4), 0, 89, add = TRUE, col = "darkblue")
curve(random_leaf_angle(x * pi / 180, 0.2), 0, 89, add = TRUE, col = "orange")
curve(random_leaf_angle(x * pi / 180, 0.4), 0, 89, add = TRUE, col = "darkred")


# Inputs

# Cosine of solar zenith angle
mu <- 0.8

# PAR and NIR albedo
alb_par <- 0.1
alb_nir <- 0.4

# Structural parameters
lai <- 3
wai <- 0.2
cai <- 1
clumping_factor <- 0.9
orient <- 0.1

# Derived parameters
phi1 <- 0.5 - orient * (0.633 + 0.33 * orient)
phi2 <- 0.877 * (1 - 2 * phi1)

# BEGIN CALCULATIONS

# Area indices
elai <- clumping_factor * lai
etai <- elai + wai

locetai <- etai / cai
leaf_weight <- elai / etai
wood_weight <- 1 - leaf_weight

# Optical depth of direct beam (lambda)
proj_area <- phi1 + phi2 * mu
lambda <- proj_area / mu

# Layer transmittance coefficients (tau) for beam and diffuse radiation
tau_beam <- (1 - cai) + cai * exp(-lambda * locetai)

ext_diff1 <- phi1 * locetai
ext_diff2 <- phi2 * locetai
tau_diff <- (1 - cai) - cai * exp(-ext_diff1 - ext_diff2) *
  (ext_diff1 ^ 2 * exp(ext_diff1) * gsl::expint_Ei(-ext_diff1) +
     (ext_diff1 - 1))

# Backscatter for direct radiation
snglscat_alb <- 0.5 * 
