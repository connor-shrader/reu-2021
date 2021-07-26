# monte-carlo.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains functions for generating data and fitting regression models using Monte Carlo simulations.
##### WARNING: This file is deprecated and was replaced by .../reu-2021/code/monte-carlo/simulation-functions.R ####

# R version: 4.1.0
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6
library(faux) # v1.0.0
library(ncvreg) # v3.13.0
library(glmnet) # v4.1-1
library(MASS) # v7.3-54

# generate_data() is used to generate data.
#
# Arguments:
#   n: Number of observations.
#   p: Number of predictors.
#   seed: Random seed to generate the data.
#   var (default 1): Vector containing the variance of each predictor. If the
#     length of var is less than p, then var is extended using rep()
#   type (default "independent"): Determines the covariance of the data.
#     "independent": No covariance.
#     "symmetric": All pairs of variables have equal covariance.
#     "autoregressive": AR(1)
#     "blockwise": Blockwise covariance.
#     "unstructured": All pairs of variables have different covariances.
#   corr (default 0): The use of corr depends on the value of type.
#     type = "independent": corr is unused.
#     type = "symmetric": Used as the value of rho,
#       the correlation any pair of predictors.
#     type = "autoregressive": Used as the value of rho in an AR(1) matrix.
#     type = "blockwise": TODO.
#     type = "unstructured": corr should be the correlation matrix.
#   beta (default NONE): The true values of the coefficient values. If beta contains
#     less than (p + 1) values, beta is extended by zero until it has length (p + 1).
#     If beta = NONE, some default coefficient values are used.
#   error_var (default 1): The variance of the random error.
generate_data <- function(n, p, seed, var = 1, type = "independent", corr = 0,
                          beta = NULL, error_var = 1) {
  set.seed(seed)
  
  # If no coefficient values were provided, generate some default values.
  if (is.null(beta)) {
    beta <- c(1, 2, -2, 0, 0, 0.5, 3)
  }
  
  # Extend or shrink the coefficient list to have length (p + 1). If beta
  # is too short, the entries added are set to zero.
  if (length(beta) < p + 1) {
    zeroes <- rep(0, (p + 1) - length(beta))
    beta <- c(beta, zeroes)
  }
  else if (length(beta) > p + 1) {
    beta <- beta[1:(p + 1)]
  }
  
  # sd will be used for the standard deviation when we call rnorm_multi() (which
  # will generate our data). sd[i] is the standard deviation for predictor i.
  sd <- rep(sqrt(var), length.out = p)
  
  # The following if-else chain will calculate r, which is used as the correlation
  # parameter for rnorm_multi().
  if (type == "independent") {
    # There is no covariance, so the correlation is zero.
    r <- 0
  }
  else if (type == "symmetric") {
    # The correlation when we call rnorm_multi will be exactly the argument
    # corr passed into this function.
    r <- corr
  }
  else if (type == "autoregressive") {
    # With AR(1), we need to generate the correlation matrix. The source for this
    # code can be found at the following link:
    # https://statisticaloddsandends.wordpress.com/2020/02/07/generating-correlation-matrix-for-ar1-model/
    
    exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - (1:n - 1))
    r <- corr^exponent
  }
  else if (type == "unstructured") {
    # If our data is unstructed, we will set r = corr. corr should contain the
    # correlation matrix already.
    r <- corr
  }
  
  # Generate the data using rnorm_multi() from the faux package. We then append
  # a column of 1's to the left of this matrix (which will correspond to the
  # intercept).
  x <- cbind(1, rnorm_multi(
    n = n,
    vars = p,
    mu = 0,
    sd = sd,
    r = r,
    as.matrix = TRUE
  ))
  
  # Generate corresponding y values for our data.
  y <- x %*% beta + rnorm(n, sd = sqrt(error_var))
  
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




# This function inputs a model and test data. It then computes and returns
# the mean squared error.
test_mse <- function(model, test_dat) {
  if (class(model) == "cv.ncvreg") { #checks for mcp or scad model
    y_hat <-  data.frame(predict(model, X = as.matrix(test_dat[,-1])))
  }
  else if (class(model) == "cv.glmnet") { #check for lasso, ridge, enet model
    y_hat <-  data.frame(predict(model, newx = as.matrix(test_dat[,-1])))
  }
  else { #rest are lm models
    y_hat <- data.frame(predict(model, newdata = test_dat[,-1]))
  }
  
  y <- test_dat[,1]
  mse <- mean(((y - y_hat)^2)[,1]) #take mean of residuals squared
  return(mse)
}


# This function inputs a result table (from calling results_table())
# and the name of a column and computes the Euclidean distance between
# results$soln and results$col. This gives an estimate for the bias for the
# coefficients of a model.
coefficient_bias <- function(results, col) {
  return(sqrt(sum((results$soln - results$col)^2)))
}



# This function combines the processes from the functions
# generate_data(), fit_models(), test_mse(), and results_table().
#
# Arguments:
#   n: Number of observations.
#   p: Number of predictors. We require that p >= 6. If p < 6, this function
#     does nothing.
#   seed: Random seed to generate the data.
monte_carlo <- function(seed, n, p, ...) {
  # Generated training AND test data.
  all.dat <- generate_data(n = n, p = p, seed = seed, ...)
  
  # The first half of the data is used for training. The other half
  # is used for testing.
  ex.dat <- all.dat[1:trunc(n / 2),]
  test.dat <- all.dat[(trunc(n / 2) + 1):n,]
  
  # Fit the models using the training data.
  models <- fit_models(ex.dat, n = n, p = p)
  
  # Generate the coefficient table and compute the mean squared error
  # for each model.
  coefs_df <- results_table(models, p = p)
  mse_list <- lapply(models, test_mse, test_dat = test.dat)
  
  return(list(coefficients = coefs_df, models = models, mse = mse_list))
}


# This helper function compares two boolean values and determines whether there
# is a true positive (tp), false negative (fn), false positive (fp), or
# true negative (tn). The first boolean is the actual value, while the second
# boolean is the predicted value. This function is used for
# generate_confusion_matrices().
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

# This function takes in a list of test results (true negative, false negative,
# false positive or true negative) and combines the sum of each result into
# a confusion matrix. Despite being called a confusion matrix, the returned
# object is a data frame. This function is used in generate_confusion_matrices().
confusion_matrix <- function(lis) {
  confusion_matrix <- data.frame(
    actual_negative = c(sum(lis == "tn"), sum(lis == "fp")),
    actual_positive = c(sum(lis == "fn"), sum(lis == "tp")),
    row.names = c("predicted_negative", "predicted_positive")
  )
  
  return(confusion_matrix)
}

# This function takes a table of coefficient estimates (from results_table())
# and computes the confusion matrix for each model. This function then returns a list
# containing these matrices.
generate_confusion_matrices <- function(coefs) {
  # Create a data frame where each entry is TRUE if the corresponding entry in
  # coefs is non-zero; otherwise, the entry is FALSE.
  coef_is_nonzero <- as.data.frame(ifelse(coefs == 0, FALSE, TRUE))
  
  # For each column of coef_is_nonzero, we compare its entries to the entries
  # in the "soln" column. This gives us a list whose entries have four possible values:
  # tn (true negative), false negative (fn), false positive (fp), or true
  # positive (tp). For example, if a certain model correctly predicts that a
  # coefficient will be non-zero, the corresponding entry will be set to "tp".
  test_results <- as.data.frame(
    apply(X = coef_is_nonzero,
          MARGIN = 2,
          FUN = function(prediction, solution) {
            mapply(compare_coefficient_estimate, solution, prediction)
          },
          solution = coef_is_nonzero$soln)
  )
  
  # For each column of test_results, compute the confusion matrix based on the
  # counts of tn, fn, fp, and tp. This then gets returned.
  matrices <- apply(X = test_results, MARGIN = 2, FUN = confusion_matrix)
  
  return(matrices)
}


seeds <- c(100:119)

# Run monte_carlo 20 times, each time with 200 observations and 10 predictors.
# results <- lapply(seeds, monte_carlo, n = 200, p = 10, type = "independent")
#(lapply(seeds, monte_carlo, n = 200, p = 10, type = "independent"))
# results2 <- lapply(seeds, monte_carlo, n = 200, p = 10, type = "symmetric")
#system.time(lapply(seeds, monte_carlo, n = 200, p = 10, type = "symmetric"))


# Calculate sample variance of the coefficients
x_dif_2 <- function(coef_df, model){
  x_hat <- coef_df[["coefficients"]][model]
  betas <- coef_df[["coefficients"]]["soln"]
  x_difference <- betas - x_hat
  return(x_difference^2)
}

sample_mean <- function(row, df){  #takes sample mean: sum(x)/(n-1)
  total <- sum(df[row,])
  n <- length(df[row, ])
  samp_avg <- total / (n-1)
  return(samp_avg)
}

sample_var <- function(model, coefs_list){
  x_dif_df <-  lapply(coefs_list, x_dif_2, model = model)
  x_dif_df <- as.data.frame(x_dif_df)
  names_list <- rownames(x_dif_df)
  coef_variance <- lapply(names_list, sample_mean, df = x_dif_df) #returns list with sample variance for each coefficient in model
  return(coef_variance)
}

# coef_sample_var <- sample_var(model = "lasso", coefs_list = results) #sample variance for coefficients of lasso model

ex.dat <- generate_data(n = 100, p = 10, seed = 1)
models <- fit_models(ex.dat, n = 100, p = 10)
results <- results_table(models, p = 10)

coefficient_bias(results$soln, models$lasso)

