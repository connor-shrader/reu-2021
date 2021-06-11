# monte-carlo.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains functions for generating data and fitting regression models
# using Monte Carlo simulations.

# R version: 4.1.0
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6

# generate_data() is used to generate data.
#
# Arguments:
#   n: Number of observations.
#   p: Number of predictors. We require that p >= 6. If p < 6, an empty
#     data frame is returned.
#   seed: Random seed to generate the data.
#   var (default 1): Variance of each variable.
#   covar (default "independent"): Determines the covariance of the data.
#     "independent": No covariance.
#     "unstructured": All pairs of variables have different covariances.
#     "symmetric": All pairs of variables have equal covariance.
#     "autoregressive": AR(1)
#     "blockwise": Blockwise covariance.
#   rho (default 0): The value of rho used for AR(1). If AR(1) is not used, then
#     rho is unused.
generate_data <- function(n, p, seed, var = 1, covar = "independent", rho = 0) {
  if (p < 6) {
    return(data.frame())
  }
  
  set.seed(seed)
  
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
fit_models <- function(dat, n, p) {
  models <- list()
  
  # Null model for forward selection
  nm <- lm(y ~ 1, data = dat)
  models[["nm"]] <- nm
  
  if (p < n) {
    # Full model for backward selection
    fm <- lm(y ~ ., data = dat)
    models[["fm"]] <- fm
    
    # AIC and BIC model selection for backward
    ab = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="backward", k=2, trace=F, steps=3000) #AIC
    models[["ab"]] <- ab
    
    bb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="backward", k=log(nrow(dat)), trace=F, steps=3000) #BIC 
    models[["bb"]] <- bb
    
    # AIC and BIC model selection for stepwise backward
    asb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="both", k=2, trace=F, steps=3000) #AIC
    models[["asb"]] <- asb
    
    bsb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="both", k=log(nrow(dat)), trace=F, steps=3000) #BIC
    models[["bsb"]] <- bsb
  }
  
  
  # AIC and BIC model selection for forward
  af = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="forward", k=2, trace=F, steps=3000) #AIC
  models[["af"]] <- af
  
  bf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="forward", k=log(nrow(dat)), trace=F, steps=3000) #BIC
  models[["bf"]] <- bf
   
  
  # AIC and BIC model selection for stepwise forward
  asf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="both", k=2, trace=F, steps=3000) #AIC
  models[["asf"]] <- asf
  
  bsf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="both", k=log(nrow(dat)), trace=F, steps=3000) #BIC  
  models[["bsf"]] <- bsf
  
  # Lasso model for variable selection
  lasso <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 1)
  models[["lasso"]] <- lasso
  
  # Ridge model for dealing with multicollinearity
  ridge <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 0)
  models[["ridge"]] <- ridge
  
  # Elastic Net model for multicollinearity and variable selection
  enet <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 0.8) #small alpha is not needed since small multicollinearity
  models[["enet"]] <- enet
  
  # MCP
  scad <- cv.ncvreg(X = dat[, -1], y = dat$y, penalty = "SCAD")
  models[["scad"]] <- scad
  
  mcp <- cv.ncvreg(X = dat[, -1], y = dat$y)
  models[["mcp"]] <- mcp
  
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

monte_carlo <- function(n, p, seed){
  set.seed(seed) # to generate the same data
  all.dat <- generate_data(n = n, p = p, seed = seed) # n> p, p>6
 
  ex.dat <- all.dat[1:trunc(n / 2),]
  
  test.dat <- all.dat[(trunc(n / 2) + 1):n,]
  
  models <- fit_models(ex.dat, n = n, p = p)
  
  mse_list <- lapply(models, calc_mse, test_dat = test.dat)
  
  coefs_df <- results_table(models, p = p)
  
  return(list(coefficients = coefs_df, mse = mse_list))
}


compare_coefficient_estimate <- function(solution, prediction) {
  if (solution & prediction) {
    return("tp")
  }
  else if (solution & !prediction) {
    return("fn")
  }
  else if (!solution & prediction) {
    return("fp")
  }
  else {
    return("tn")
  }
}

confusion_matrix <- function(lis) {
  confusion_matrix <- data.frame(
    actual_negative = c(sum(lis == "tn"), sum(lis == "fp")),
    actual_positive = c(sum(lis == "fn"), sum(lis == "tp")),
    row.names = c("predicted_negative", "predicted_positive")
  )
  
  return(confusion_matrix)
}

generate_confusion_matrices <- function(coefs) {
  tf_table <- as.data.frame(ifelse(coefs == 0, FALSE, TRUE))
  test_results <- as.data.frame(
    apply(X = tf_table,
          MARGIN = 2,
          FUN = function(prediction, solution) {
            mapply(compare_coefficient_estimate, solution, prediction)
          },
          solution = tf_table$soln)
  )
  
  matrices <- apply(X = test_results, MARGIN = 2, FUN = confusion_matrix)
  
  return(matrices)
}


seeds <- list(100:110)

# results <- lapply(seeds[[1]], monte_carlo)
d <- monte_carlo(n = 100, p = 10, seed = 11)
cm <- generate_confusion_matrices(d$coefficients)