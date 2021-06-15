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

dat <- monte_carlo(n = 100, p = 2, iterations = 5, type = "unstructured", )
#View(dat$coefficients[[1]])
conf_matrices <- generate_confusion_matrices(dat$coefficients)
# coef_bias <- coefficient_bias(dat[[1]]$coefficients, "lasso")