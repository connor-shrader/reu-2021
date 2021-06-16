# This file contains code to "compute" bias and variance for a collection of
# models. I don't know if it's correct. - Connor

mean_coefficient_estimates <- lapply(dat, function(iter) {iter$coefficients$scad})

euclidean_distance <- function(v1, v2) {
  { sum((v1 - v2)^2) }
}

all_coefs <- lapply(dat, function(iter) {iter$coefficients})
mean_coefficient_estimates <- Reduce("+", all_coefs) / length(all_coefs)
bias <- apply(X = mean_coefficient_estimates, 
              FUN = euclidean_distance,
              MARGIN = 2,
              v2 = mean_coefficient_estimates$soln)

variance_for_one_table <- function(coefficients, mean_coefficients) {
  mapply(euclidean_distance, coefficients, mean_coefficients)
}

difference <- lapply(dat,
                     function(iter) { variance_for_one_table(iter$coefficients,
                                                             mean_coefficients = mean_coefficient_estimates)})

variance <- Reduce("+", difference) / length(difference)
