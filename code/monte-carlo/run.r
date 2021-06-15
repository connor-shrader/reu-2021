# monte-carlo.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains anything that we with to run.

# R version: 4.1.0
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6
library(faux) # v1.0.0
library(ncvreg) # v3.13.0
library(glmnet) # v4.1-1
library(MASS) # v7.3-54

source("simulation.r")
source("metrics.r")

dat <- monte_carlo(n = 100, p = 10, iterations = 5, beta = c(1, 1, 1),
                   type = "autoregressive", corr = 0.9, error_var = 50)
View(dat[[1]]$coefficients)
conf_matrices <- generate_confusion_matrices(dat[[1]]$coefficients)
