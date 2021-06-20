# simulation.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains functions for generating data and fitting regression models
# using Monte Carlo simulations.

# FUNCTIONS:
#   generate_coefficients() (Helper function)
#   generate_data()
#   fit_models()
#   model_data_frame() (Helper function)
#   results_table()
#   monte_carlo_single_iteration (Helper function)
#   monte_carlo()

# R version: 4.1.0
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6
library(faux) # v1.0.0
library(ncvreg) # v3.13.0
library(glmnet) # v4.1-1
library(MASS) # v7.3-54


# This helper function takes in a vector beta and the number of
# predictors p. If beta is NULL, then this function returns a default
# vector containing some coefficient values. If beta is not NULL, then beta
# is shortened or extended to have (p + 1) values. Any entries that are added
# will have a value of zero.
# This function is used to create a valid coefficient vector which is used to
# generate simulated data in generate_data().
generate_coefficients <- function(beta, p) {
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
  
  return(beta)
}



# generate_data() is used to generate data.
#
# Arguments:
#   n: Number of observations.
#   p: Number of predictors.
#   beta (default NONE): The true values of the coefficient values. If beta contains
#     less than (p + 1) values, beta is extended by zero until it has length (p + 1).
#     If beta = NONE, some default coefficient values are used.
#   sd (default 1): The standard deviation of the random error.
#   type (default "independent"): Determines the covariance of the data.
#     "independent": No covariance.
#     "symmetric": All pairs of variables have equal covariance.
#     "autoregressive": AR(1)
#     "blockwise": Blockwise covariance.
#     "unstructured": All pairs of variables have different covariances.
#   corr (default 0): The use of corr depends on the value of the argument "type".
#     type = "independent": corr is unused.
#     type = "symmetric": Used as the value of rho,
#       the correlation any pair of predictors.
#     type = "autoregressive": Used as the value of rho in an AR(1) matrix.
#     type = "blockwise": The correlation of predictors that are in the same block.
#       Predictors in different blocks have correlation zero.
#     type = "unstructured": corr should be the correlation matrix.
#   block_size (default NULL): The size of each block if using blockwise correlation.
#     block_size should divide the number of predictors.
#   seed (default NULL): Random seed to generate the data. If NULL, no seed it set.
generate_data <- function(n, p, beta = NULL, type = "independent", corr = 0,
                          sd = 1, block_size = NULL, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generate the coefficient values if beta is NULL or does not have the right
  # length.
  beta <- generate_coefficients(beta, p)
  
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
    
    exponent <- abs(matrix(1:p - 1, nrow = p, ncol = p, byrow = TRUE) - (1:p - 1))
    r <- corr^exponent
  }
  else if (type == "blockwise") {
    # The correlation matrix will be a block diagonal matrix. a_ij = 0 if i = j,
    # a_ij = corr if i and j are in the same block, and a_ij = 0 otherwise.
    
    if (is.null(block_size)) {
      stop("block size must be given a value")
    }
    else if (p %% block_size != 0) {
      stop("block_size should divide p")
    }
    
    # To generate the correct matrix, we take the Kronecker product of the 
    # (p / block_size) * (p / block_size) identity matrix and a matrix representing
    # one block.
    r <- kronecker(diag(p / block_size),
                   matrix(rep(c(1, rep(corr, times = block_size)),
                          length.out = block_size^2),
                          nrow = block_size, ncol = block_size))
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
  y <- x %*% beta + rnorm(n, sd = sd)
  
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
  print("nm")
  nm <- lm(y ~ 1, data = dat)
  models[["nm"]] <- nm
  
  if (2 * p < n) {
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
  }
  
  print("lasso")
  # Lasso model for variable selection
  lasso <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 1)
  models[["lasso"]] <- lasso
  
  # Ridge model for dealing with multicollinearity
  ridge <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 0)
  models[["ridge"]] <- ridge
  
  # Elastic Net model for multicollinearity and variable selection
  enet <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$y, alpha = 0.8) #small alpha is not needed since small multicollinearity
  models[["enet"]] <- enet
  
  # SCAD
  scad <- cv.ncvreg(X = dat[, -1], y = dat$y, penalty = "SCAD")
  models[["scad"]] <- scad
  
  # MCP
  mcp <- cv.ncvreg(X = dat[, -1], y = dat$y)
  models[["mcp"]] <- mcp
  
  print("rf")
  
  ## Grid Search Random Forest ##
  num_p <- length(dat) - 1
  
  rf_grid.h2o <- list(
    ntrees      = c(200, 500, 1500),
    mtries      = c(round(num_p /2), round(num_p / 3))#,
    #max_depth   = c(20, 30, 40),
    #sample_rate = c(.55, .632, .75)
  )
  
  # random grid search criteria
  search_criteria <- list(
    strategy = "RandomDiscrete",
    stopping_metric = "mse",
    stopping_tolerance = 0.005,
    stopping_rounds = 10,
    max_runtime_secs = 30*60
  )
  
  # build grid search 
  rf_grid <- h2o.grid(
    algorithm = "randomForest",
    y = "y", 
    training_frame = as.h2o(dat),
    hyper_params = rf_grid.h2o,
    search_criteria = search_criteria
  )
  
  # collect the results and sort by our model performance metric of choice
  rf_grid_perf <- h2o.getGrid(
    grid_id = rf_grid@grid_id, 
    sort_by = "mse", 
    decreasing = FALSE
  )
  
  # Grab the model_id for the top model, chosen by validation error
  rf_best_model_id <- rf_grid_perf@model_ids[[1]]
  rf_best_model <- h2o.getModel(rf_best_model_id)
  models[["rf"]] <- rf_best_model
  
  print("gbm")
  ##Grid Search gbm##
  # create hyperparameter grid
  hyper_grid <- list(
    max_depth = c(1, 3, 5),
    min_rows = c(1, 5, 10),
    learn_rate = c(0.01, 0.05, 0.1),
    ntrees = c(500, 2000, 5000)
    #learn_rate_annealing = c(.99, 1),
    #sample_rate = c(.5, .75, 1),
    #col_sample_rate = c(.8, .9, 1)
  )
  
  gbm_search_criteria <- list(
    strategy = "RandomDiscrete",
    stopping_metric = "mse",
    stopping_tolerance = 0.005,
    stopping_rounds = 2,
    max_runtime_secs = 60*60
  )
  
  # perform grid search for gbm
  gbm_grid <- h2o.grid(
    algorithm = "gbm",
    y = "y", 
    training_frame = as.h2o(dat[1:80, ]),
    validation_frame = as.h2o(dat[81:100, ]),
    hyper_params = hyper_grid,
    search_criteria = gbm_search_criteria,
    stopping_rounds = 10,
    # stopping_tolerance = 0,
    seed = 123
  )
  
  # collect the results and sort by our model performance metric of choice
  gbm_grid_perf <- h2o.getGrid(
    grid_id = gbm_grid@grid_id, 
    sort_by = "mse", 
    decreasing = TRUE
  )
  
  View(gbm_grid_perf)
  #gbm_grid_perf
  
  # Grab the model_id for the top model, chosen by validation error
  gbm_best_model_id <- gbm_grid_perf@model_ids[[1]]
  gbm_best_model <- h2o.getModel(gbm_best_model_id)
  models[["gbm"]] <- gbm_best_model
  
  return(models)
}



# This helper function takes in a model and a name for the model as parameters.
# It returns a data frame containing the coefficient estimates using that model
# as well as the names of the corresponding variables. This function is used in
# results_table().
model_data_frame <- function(model, model_name) {
  if (class(model) %in% c("cv.glmnet", "lm", "cv.ncvreg")){
    # Create a data frame with the coefficient estimates.
    df <- data.frame(as.matrix(coef(model)))
    
    # Rename the column to have the correct model name.
    colnames(df) <- model_name
    
    # Add a row containing the name of the variable corresponding to each coefficient.
    df$row_names <- row.names(df)
    return(df)
  }
}



# This function takes in a named list of models and the number of predictors.
# It returns a data frame containing the coefficient estimates for each model.
results_table <- function(models, beta, p) {
  row_names <- c("(Intercept)", paste("x", 1:p, sep = ""))
  
  # Create a dataframe with two columns. The first column are the variable names
  # ((Intercept), x1, x2, ..., xp). The second column contains the actual
  # coefficient values.
  results <- data.frame(row_names = row_names, soln = generate_coefficients(beta, p))
  
  # This loop iterates through each model and creates a dataframe containing the
  # coefficient estimates for that model. Then, this dataframe is joined with df.
  # At the end of the loop, df contains the coefficients from all models.
  for (i in 1:length(models)) {
    if (class(models[[i]]) %in% c("cv.glmnet", "lm", "cv.ncvreg")){
      results <- left_join(results, model_data_frame(models[[i]], names(models)[[i]]),
                           by = "row_names",
                           all.x = TRUE
    )
    }
  }
  
  # Set the row names for df to the column called row_names.
  row.names(results) <- results$row_names
  
  # Remove the row_names column from df. We needed this column earlier in order to
  # run left_join. We return the resulting data.frame.
  results[is.na(results)] <- 0
  return(results[, -1])
}



# This function combines the processes of generate_data(), fit_models(), and
# results_table(). This function takes in any inputs that go into generate_data(),
# and this function outputs a list containing the outputs of fit_models()
# and results_table().
full_simulation <- function(n, p, beta = NULL, seed = NULL, ...) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Generate training and test data.
  train_data <- generate_data(n = n, p = p, beta = beta, ...)
  test_data <- generate_data(n = n, p = p, beta = beta, ...)
  
  # Fit the models using the training data.
  models <- fit_models(train_data, n = n, p = p)
  
  # Generate the coefficient table and compute the mean squared error
  # for each model.
  coefficients_table <- results_table(models, beta = beta, p = p)
  
  train_mse <- lapply(models, mean_squared_error, test_dat = train_data)
  test_mse <- lapply(models, mean_squared_error, test_dat = test_data)
  
  confusion_matrices <- confusion_matrices(coefficients_table)
  
  return(list(
    coefficients = coefficients_table, 
    models = models, 
    train_mse = train_mse, 
    test_mse = test_mse, 
    confusion_matrices = confusion_matrices))
}



# This function iterates the monte_carlo_single_iteration() function multiple times.
# The seed is automatically set for each iteration. If iterations == 1, this function
# returns the same thing as calling monte_carlo_single_simulation(seed = 1);
# otherwise, this function returns a list of repeated calls to
# monte_carlo_single_simulation.
monte_carlo <- function(n, p, iterations, beta = NULL, ...) {
  set.seed(1)
  rep(full_simulation(n = n, p = p, beta = beta, ...), times = iterations)
}