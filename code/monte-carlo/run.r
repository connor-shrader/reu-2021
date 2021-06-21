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

# Used for stepwise selection.
library(MASS) # v7.3-54

# Used for confusion matrices.
library(caret) # v6.0-88

# Used to set the current working directory to this script.
library(rstudioapi) # v0.13
library(bit64) # v4.0.5

# Used for XGBoost models.
library(xgboost) # v1.4.1.1

# Used for random forest models.
library(ranger) # v0.12.1

# Support vector machine model
library(e1071) # v1.7-7

setwd(dirname(getActiveDocumentContext()$path))
source("simulation.r")
source("metrics.r")

#random_data <- generate_data(seed = 1, n = 100, p = 10, corr = 0, type = "independent")
#models <- fit_models(dat = random_data, n = 100, p = 10)

#test_data <- generate_data(seed = 1, n = 100, p = 10, corr = 0, type = "independent")

#lasso_pred <- predict(models$lasso, as.matrix(test_data[, -1]))
#lasso_eval <- data.frame(lasso_pred, test_data[, 1])
#lasso_mse <- 1/100 * sum((lasso_eval[, 1] - lasso_eval[, 2])^2)

#rf_pred <- h2o.performance(models$rf, newdata = as.h2o(test_data))

#rf_pred <- as.data.frame(predict(object = models$rf, newdata = as.h2o(test_data[, -1])))
#rf_eval <- data.frame(rf_pred, test_data[, 1])
#rf_mse <- 1/100 * sum((rf_eval[, 1] - rf_eval[, 2])^2)

#rf_eval[3] <- lasso_eval[1]
#colnames(rf_eval) <- c("rf", "true", "lasso")













system.time(dat <- monte_carlo(n = 300,
                   p = 10,
                   type = "symmetric",
                   corr = 0.5,
                   sd = 1,
                   iterations = 1,
                   seed = 1
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
