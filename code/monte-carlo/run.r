# monte-carlo.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains everything to be executed using the functions from
# simulation.r and metrics.r.

# R version: 4.1.0
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6
library(faux) # v1.0.0
library(ncvreg) # v3.13.0
library(glmnet) # v4.1-1
library(MASS) # v7.3-54

source("simulation.r")
source("metrics.r")


dat <- monte_carlo(n = 100, p = 10, iterations = 50, beta = c(1, 1, 1), error_var = 0.2)
# View(dat[[1]]$coefficients)
conf_matrices <- confusion_matrices(dat[[1]]$coefficients)
# View(conf_matrices)

# plot CV-error vs. lambda for scad.
# plot(dat[[1]]$models$scad)

# mean_coefficient_estimates <- lapply(dat, function(iter) {iter$coefficients$scad})

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

# difference <- lapply(dat, euclidean_distance, v2 )