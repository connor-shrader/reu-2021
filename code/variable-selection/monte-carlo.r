# monte-carlo.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains functions for generating data and fitting regression models
# using Monte Carlo simulations.

library(tidyverse)
library(dplyr)

# generate_data() is used to generate data.
#
# Arguments:
#   n: Number of observations.
#   p: Number of predictors. We require that p >= 6. If p < 6, an empty
#     data frame is returned.
#   var (default 1): Variance of each variable.
#   covar (default "independent"): Determines the covariance of the data.
#     "independent": No covariance.
#     "unstructured": All pairs of variables have different covariances.
#     "symmetric": All pairs of variables have equal covariance.
#     "autoregressive": AR(1)
#     "blockwise": Blockwise covariance.
#   rho (default 0): The value of rho used for AR(1). If AR(1) is not used, then
#     rho is unused.
generate_data <- function(n, p, var = 1, covar = "independent", rho = 0) {
  if (p < 6) {
    return(data.frame())
  }
  
  # Generate coefficient values.
  beta <- c(1, 2, -2, 0, 0, 0.5, 3, rep(0, (p-6)))
  
  if (covar == "independent") {
    x <- cbind(1, matrix(rnorm(n * p), nrow = n, ncol = p))
  }
  
  # Generate corresponding y values.
  y <- x %*% beta + rnorm(n, sd = sqrt(var))
  
  # Create return data frame. We removed the column of 1's that we used as
  # an intercept for generating the data.
  dat <- data.frame(cbind(y, x[, -1]))
  
  # Set the column names to "y, x1, x2, ..., xp"
  colnames(dat) <- c("y", paste("x", 1:p, sep=""))
  return(dat)
}



# This function takes in a data frame of generated data (using generate_data())
# and fits various regression models. This function then returns a list of the
# models.
fit_models <- function(dat) {
  # Full model for backward selection
  fm <- lm(y ~ ., data = dat)
  
  # Null model for forward selection
  nm <- lm(y ~ 1, data = dat)
  
  # AIC and BIC model selection for forward
  af = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="forward", k=2, trace=F, steps=3000) #AIC
  bf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="forward", k=log(nrow(dat)), trace=F, steps=3000) #BIC
  
  # AIC and BIC model selection for backward
  ab = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="backward", k=2, trace=F, steps=3000) #AIC
  bb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="backward", k=log(nrow(dat)), trace=F, steps=3000) #BIC  
  
  # AIC and BIC model selection for stepwise forward
  asf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="both", k=2, trace=F, steps=3000) #AIC
  bsf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="both", k=log(nrow(dat)), trace=F, steps=3000) #BIC  
  
  # AIC and BIC model selection for stepwise backward
  asb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="both", k=2, trace=F, steps=3000) #AIC
  bsb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="both", k=log(nrow(dat)), trace=F, steps=3000) #BIC 
  
  # Lasso model for variable selection
  lasso <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 1)
  
  # Ridge model for dealing with multicollinearity
  ridge <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 0)
  
  # Elastic Net model for multicollinearity and variable selection
  enet <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 0.8) #small alpha is not needed since small multicollinearity
  
  # MCP
  scad <- cv.ncvreg(X = dat[, -1], y = dat$y, penalty = "SCAD")
  
  mcp <- cv.ncvreg(X = dat[, -1], y = dat$y)
  
  models <- list(fm, af, bf, ab, bb, asf, bsf, asb, bsb, mcp, scad, lasso, ridge, enet)
  names(models) <- c("fm", "af", "bf", "ab", "bb", "asf", "bsf", "asb", "bsb", "mcp", "scad", "lasso", "ridge", "enet")
  
  return(models)
}



# This helper function takes in a model and a name for the model as parameters.
# It returns a data frame containing the coefficient estimates using that model
# as well as the names of the corresponding variables. This function is used in
# results_table().
model_data_frame <- function(model, model_name) {
  # Create a data frame with the coefficient estimates.
  df <- data.frame(as.matrix(coef(model)))
  
  # Rename the column to have the correct model name.
  colnames(df) <- model_name
  
  # Add a row containing the name of the variable corresponding to each coefficient.
  df$row_names <- row.names(df)
  df
}

# This function takes in a named list of models and the number of predictors.
# It returns a data frame containing the coefficient estimates for each model.
results_table <- function(models, p) {
  row_names <- c("(Intercept)", paste("x", 1:p, sep = ""))
  
  # Create a dataframe with two columns. The first column are the variable names
  # ((Intercept), x1, x2, ..., xp). The second column contains the actual
  # coefficient values.
  results <- data.frame(row_names = row_names, soln = c(1, 2, -2, 0, 0, 0.5, 3, rep(0, (p-6))))
  
  # This loop iterates through each model and creates a dataframe containing the
  # coefficient estimates for that model. Then, this dataframe is joined with df.
  # At the end of the loop, df contains the coefficients from all models.
  for (i in 1:length(models)) {
    results <- left_join(results, model_data_frame(models[[i]], names(models)[[i]]),
                    by = "row_names",
                    all.x = TRUE
    )
  }
  
  # Set the row names for df to the column called row_names.
  row.names(results) <- results$row_names
  
  # Remove the row_names column from df. We needed this column earlier in order to
  # run left_join. We return the resulting data.frame.
  results[is.na(results)] <- 0
  return(results[, -1])
}



#####Monte Carlo Replication Function #####
calc_mse <- function(model, test_dat) {
  if (class(model) == "cv.ncvreg") { #checks for mcp or scad model
    y_hat <-  data.frame(predict(model, X = as.matrix(test_dat[,-1])))
  }
  else if (class(model) == "cv.glmnet") { #check for lasso, ridge, enet model
    y_hat <-  data.frame(predict(model, newx = as.matrix(test_dat[,-1])))
  }
  else { #rest is lm models
    y_hat <- data.frame(predict(model, newdata = test_dat[,-1]))
  }
  
  y <- test_dat[,1]
  mse <- mean(((y - y_hat)^2)[,1]) #take mean of residuals squared
  return(mse)
}

monte_carlo <- function(seed){
  set.seed(seed) # to generate the same data
  all.dat <- generate_data(n=200, p = 10) # n> p, p>6
 
  ex.dat <- all.dat[1:100,]
  
  test.dat <- all.dat[101:nrow(all.dat), ]
  
  models <- fit_models(ex.dat)
  
  mse_list <- lapply(models, calc_mse, test_dat = test.dat)
  
  print(names(models))
  coefs_df <- results_table(models, p = 10)
  
  return(list(coefs_df, mse_list))
}

seeds <- list(100:110)

# results <- lapply(seeds[[1]], monte_carlo)
d <- monte_carlo(35246)




#row_names <- c("(Intercept)", paste("x", 1:p, sep = ""))
#models <- list(fm, af, bf, ab, bb, asf, bsf, asb, bsb, mcp, scad, lasso, ridge, enet)
#names(models) <- c("fm", "af", "bf", "ab", "bb", "asf", "bsf", "asb", "bsb", "mcp", "scad", "lasso", "ridge", "enet")
#df <- create_table(models)
