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
library(caret)
library(rstudioapi)
library(h2o)

setwd(dirname(getActiveDocumentContext()$path))
source("simulation.r")
source("metrics.r")

# Initiate h2o
h2o.no_progress()
h2o.init(max_mem_size = "5g", ip = "localhost", startH2O = TRUE)  #start up h2o, use 5gb of ram


dat <- monte_carlo(n = 200,
                    p = 500,
                    iterations = 1,
                    type = "autoregressive",
                   corr = 0.9,
                    error_var = 1,
)
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

n <- c(200)
p <- c(2000)
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
  
  results <- monte_carlo(n = n,
                         p = p,
                         iterations = iterations,
                         error_var = error_var,
                         type = type,
                         corr = corr)
  
  save(results, file = paste("../../data/simulations/sim_results_", n, "_", p, "_", error_var, "_", type, "_", corr, ".Rdata", sep = ""))
}

system.time(apply(X = parameters, MARGIN = 1, FUN = run_simulations))

# res <- monte_carlo(n = 100, p = 10, iterations = 5, error_var = 1, type = "independent", corr = 1)
