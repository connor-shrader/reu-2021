# monte-carlo.r
# Gabe Ackall, Connor Shrader

# This file contains any temporary code that we needed to run to test the
# functions in metrics.r and simulation.r.

rm(list = ls())

# Used to set current working directory to the location of this file.
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("simulation.r")
source("metrics.r")

system.time(dat <- monte_carlo(n = 100,
                   p = 10,
                   type = "autoregressive",
                   corr = 0.9,
                   st_dev = 1,
                   iterations = 1,
                   block_size = 5
))

View(dat[[1]]$coefficients)
conf_matrices <- confusion_matrices(dat[[1]]$coefficients)
View(conf_matrices)

# plot CV-error vs. lambda for scad.
# plot(dat[[1]]$models$scad)

# n <- c(50, 200, 1000)
# p <- c(10, 100, 2000)
# sigma <- c(1, 3, 6)
# covar <- c("independent", "symmetric", "autoregressive")
# rho <- c(0.2, 0.5, 0.9)

# p == 100: 4 blocks of 25 predictors
# p

n <- c(100)
p <- c(10)
sigma <- c(1)
covar <- c("independent")
rho <- c(0.2)

parameters <- expand.grid(n, p, sigma, covar, rho)
colnames(parameters) <- c("n", "p", "sigma", "covar", "rho")

# Delete extraneous rows caused by having covar = "independent." For
# rows with covar = "independent", set their rho to 0 (this is optional).
parameters <- parameters[!(parameters$covar == "independent" & parameters$rho != 0.2), ]
parameters$rho <- ifelse(parameters$covar == "independent",
                         0,
                         parameters$rho)

# Not needed
# parameters2[parameters$covar == "independent" & parameters$rho != 0.2]$rho <- 0

run_simulations <- function(row) {
  print(row)
  iterations <- 1
  n <- as.numeric(row["n"])
  p <- as.numeric(row["p"])
  error_var <- as.numeric(row["sigma"])^2
  type <- row["covar"]
  corr <- as.numeric(row["rho"])
  
  if (type == "blockwise") {
    if (p == 10) {
      block_size <- 5
    }
    else if (p == 100) {
      block_size <- 25
    }
    else if (p == 2000) {
      block_size <- 100
    }
  }
  
  results <- monte_carlo(n = n,
                         p = p,
                         iterations = iterations,
                         error_var = error_var,
                         type = type,
                         corr = corr)
  
  save(results, file = paste("../../data/simulations/sim_results_", n, "_", p, "_", error_var, "_", type, "_", corr, ".Rdata", sep = ""))
}

system.time(apply(X = parameters, MARGIN = 1, FUN = run_simulations))

# res <- monte_carlo(n = 100, p = 10, iterations = 5, type = "independent", corr = 1)
