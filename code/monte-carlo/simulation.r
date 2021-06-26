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
#   full_simulation (Helper function)
#   repeat_simulation_until_successful (Helper function)
#   monte_carlo()

# R version: 4.1.0

# Used for data cleaning
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6

# Used for multi-normal distribution.
library(faux) # v1.0.0

# SCAD and MCP models
library(ncvreg) # v3.13.0

# Used for LASSO, ridge
library(glmnet) # v4.1-1

# Used for adaptive elastic net regression
library(gcdnet) #v1.0.5

# Used for stepwise selection.
library(MASS) # v7.3-54

# Used for confusion matrices.
library(caret) # v6.0-88

# Used for XGBoost models.
library(xgboost) # v1.4.1.1

# Used for random forest models.
library(ranger) # v0.12.1

# Support vector machine model
library(e1071) # v1.7-7

# Used for parallel processing
library(parallel) # v4.1.0


# This helper function takes in a vector beta and the number of
# predictors p. If beta is NULL, then this function returns a default
# vector containing some coefficient values. If beta is not NULL, then beta
# is shortened or extended to have (p + 1) values. Any entries that are added
# to beta will have a value of zero.
# This function is used to create a valid coefficient vector which is used to
# generate simulated data in generate_data(). This function is also needed
# to create a results table using results_table().
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
#   beta (default NONE): The true values of the coefficients. The function
#     generate_coefficients() is called to ensure that beta is a vector with
#     (p + 1) coefficients. If beta contains less than (p + 1) values, beta
#     is extended by zeros until it has length (p + 1). If beta = NONE, some
#     default coefficient values are used.
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
#   st_dev (default 1): The standard deviation of the random error.
#   block_size (default NULL): The size of each block if using blockwise correlation.
#     block_size should divide the number of predictors.
#   seed (default NULL): Random seed to generate the data. If NULL, no seed it set.
generate_data <- function(n, p, beta = NULL, type = "independent", corr = 0,
                          st_dev = 1, block_size = NULL) {
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
    sd = st_dev,
    r = r,
    as.matrix = TRUE
  ))
  
  # Generate corresponding y values for our data.
  y <- x %*% beta + rnorm(n, sd = st_dev)
  
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
  runtimes <- list()
  
  # Null model for forward selection
  nm_time <- system.time(nm <- lm(y ~ 1, data = dat))
  models[["nm"]] <- nm
  runtimes[["nm"]] <- nm_time
  
  if (2 * p < n) {
    # Full model for backward selection
    fm_time <- system.time(fm <- lm(y ~ ., data = dat))
    models[["fm"]] <- fm
    runtimes[["fm"]] <- fm_time
    
    if (p <= 40) {
      # AIC and BIC model selection for backward
      ab_time <- system.time(ab <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
                                            direction="backward", k=2, trace=F,
                                            steps=3000)) #AIC
      models[["ab"]] <- ab
      runtimes[["ab"]] <- ab_time
      
      bb_time <- system.time(bb <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
                                            direction="backward", k=log(nrow(dat)),
                                            trace=F, steps=3000)) #BIC 
      models[["bb"]] <- bb
      runtimes[["bb"]] <- bb_time
      
      # AIC and BIC model selection for stepwise backward
      asb_time <- system.time(asb <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
                                              direction="both", k=2, trace=F,
                                              steps=3000)) #AIC
      models[["asb"]] <- asb
      runtimes[["asb"]] <- asb_time
      
      bsb_time <- system.time(bsb <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
                                              direction="both", k=log(nrow(dat)),
                                              trace=F, steps=3000)) #BIC
      models[["bsb"]] <- bsb
      runtimes[["bsb"]] <- bsb_time
    }
    
    # AIC and BIC model selection for forward
    af_time <- system.time(af <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
                                          direction="forward", k=2, trace=F,
                                          steps=3000)) #AIC
    models[["af"]] <- af
    runtimes[["af"]] <- af_time
    
    bf_time <- system.time(bf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
                                          direction="forward", k=log(nrow(dat)),
                                          trace=F, steps=3000)) #BIC
    models[["bf"]] <- bf
    runtimes[["bf"]] <- bf_time
    
    # AIC and BIC model selection for stepwise forward
    asf_time <- system.time(asf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
                                            direction="both", k=2, trace=F,
                                            steps=3000)) #AIC
    models[["asf"]] <- asf
    runtimes[["asf"]] <- asf_time
    
    bsf_time <- system.time(bsf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
                                            direction="both", k=log(nrow(dat)),
                                            trace=F, steps=3000)) #BIC  
    models[["bsf"]] <- bsf
    runtimes[["bsf"]] <- bsf_time
    
    # Ridge model for dealing with multicollinearity
    ridge_time <- system.time(ridge <- cv.glmnet(x = as.matrix(dat[,-1]),
                                                 y = dat$y, alpha = 0))
    models[["ridge"]] <- ridge
    runtimes[["ridge"]] <- ridge_time
  }
  
  # Lasso model for variable selection
  lasso_time <- system.time(lasso <- cv.glmnet(x = as.matrix(dat[,-1]),
                                               y = dat$y, alpha = 1))
  models[["lasso"]] <- lasso
  runtimes[["lasso"]] <- lasso_time
  
  # Elastic Net model for multicollinearity and variable selection
  enet_time <- system.time(enet <- cv.glmnet(x = as.matrix(dat[,-1]), 
                                             y = dat$y, alpha = 0.8)) #small alpha is not needed since small multicollinearity
  models[["enet"]] <- enet
  runtimes[["enet"]] <- enet_time
  
  # Adaptive Ridge
  adap_ridge_time <- system.time(adap_ridge <- cv.gcdnet(x = as.matrix(dat[,-1]),
                                                         y = dat$y, nfolds = 10,
                                                         method = "ls", lambda = 0))
  models[["adap_ridge"]] <- adap_ridge
  runtimes[["adap_ridge"]] <- adap_ridge_time
  
  # Adaptive Lasso
  adap_lasso_time <- system.time(adap_lasso <- cv.gcdnet(x = as.matrix(dat[,-1]),
                                                         y = dat$y, nfolds = 10,
                                                         method = "ls", lambda2 = 0))
  models[["adap_lasso"]] <- adap_lasso
  runtimes[["adap_lasso"]] <- adap_lasso_time
  
  # Adaptive Elastic Net model for variable selection and multicollinearity
  adap_enet_time <- system.time(adap_enet <- cv.gcdnet(x = as.matrix(dat[,-1]),
                                                       y = dat$y, nfolds = 10,
                                                       method = "ls"))
  models[["adap_enet"]] <- adap_enet
  runtimes[["adap_enet"]] <- adap_enet_time
  
  # SCAD
  scad_time <- system.time(scad <- cv.ncvreg(X = dat[, -1], y = dat$y, penalty = "SCAD"))
  models[["scad"]] <- scad
  runtimes[["scad"]] <- scad_time
  
  # MCP
  mcp_time <-  system.time(mcp <- cv.ncvreg(X = dat[, -1], y = dat$y))
  models[["mcp"]] <- mcp
  runtimes[["mcp"]] <- mcp_time
  
  # Separate data into x and y matrices. This is needed to run xgboost.
  train_x_data <- as.matrix(dat[, -1])
  train_y_data <- as.matrix(dat[, 1])
  
  # XGBoost Grid Search
  xgb_time <- system.time({
  xgb_hyper_grid <- expand.grid(
    eta = c(.1, .3, .5),  # learning rate
    max_depth = c(1, 3, 7), # maximum depth of each tree
    optimal_trees = 0,               # a place to dump results
    min_RMSE = 0                     # a place to dump results
  )

  # lapply(1:nrow(xgb_hyper_grid), function(i) {
  #   # create parameter list
  #   params <- list(
  #     eta = xgb_hyper_grid$eta[i],
  #     max_depth = xgb_hyper_grid$max_depth[i],
  #     min_child_weight = xgb_hyper_grid$min_child_weight[i],
  #     subsample = xgb_hyper_grid$subsample[i],
  #     colsample_bytree = xgb_hyper_grid$colsample_bytree[i]
  #   )
  # 
  #   # train model
  #   xgb.tune <- xgb.cv(
  #     params = params,
  #     data = train_x_data,
  #     label = train_y_data,
  #     nrounds = 1000,
  #     nfold = 5,
  #     objective = "reg:squarederror",  # for regression models
  #     verbose = 0,               # silent,
  #     early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
  #   )
  # 
  #   # add min training error and trees to grid
  #   xgb_hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  #   xgb_hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
  # })
  
  # grid search
  for(i in 1:nrow(xgb_hyper_grid)) {

    # create parameter list
    params <- list(
      eta = xgb_hyper_grid$eta[i],
      max_depth = xgb_hyper_grid$max_depth[i]
    )

    # train model
    xgb.tune <- xgb.cv(
      params = params,
      min_child_weight = 1, 
      subsample = 1, 
      colsample_bytree = 1,
      data = train_x_data,
      label = train_y_data,
      nrounds = 1000,
      nfold = 5,
      objective = "reg:squarederror",  # for regression models
      verbose = 0,               # silent,
      early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
    )

    # add min training error and trees to grid
    xgb_hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
    xgb_hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
  }
  
  xgb_best_grid <- xgb_hyper_grid %>%
    dplyr::arrange(min_RMSE)
  
  xgb_best_params <- list(
    eta = xgb_best_grid$eta[1],
    max_depth = xgb_best_grid$max_depth[1]
  )
  
  train_set <- xgb.DMatrix(data = as.matrix(dat[, -1]), label = as.matrix(dat[, 1]))
  
  # train best model
  xgb.best <- xgboost(
    params = xgb_best_params,
    data = train_set,
    nrounds = 1000,
    objective = "reg:squarederror",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
  )
  })
  models[["gbm"]] <- xgb.best
  runtimes[["gbm"]] <- xgb_time
  
  # Random Forest Grid Search
  rf_time <- system.time({
  
  rf_hyper_grid <- expand.grid(
    mtry       = c(floor(sqrt(p)), floor(p / 3), floor(p / 2)),  # Predictors per tree
    n.trees    = c(300, 400, 500, 600),
    OOB_RMSE   = 0
  )
  
  # lapply(1:nrow(rf_hyper_grid), function(i) {
  #   # train model
  #   rf_model <- ranger(
  #     formula         = y ~ ., 
  #     data            = dat, 
  #     num.trees       = rf_hyper_grid$n.trees[i],
  #     mtry            = rf_hyper_grid$mtry[i],
  #     min.node.size   = rf_hyper_grid$node_size[i],
  #     sample.fraction = rf_hyper_grid$sampe_size[i],
  #     seed            = 123
  #   )
  #   
  #   # add OOB error to grid
  #   rf_hyper_grid$OOB_RMSE[i] <- sqrt(rf_model$prediction.error)
  # })
  
  for(i in 1:nrow(rf_hyper_grid)) {

    # train model
    rf_model <- ranger(
    formula         = y ~ .,
      data            = dat,
      num.trees       = rf_hyper_grid$n.trees[i],
      mtry            = rf_hyper_grid$mtry[i],
      min.node.size   = 5,
      seed            = 123
    )

    # add OOB error to grid
    rf_hyper_grid$OOB_RMSE[i] <- sqrt(rf_model$prediction.error)
  }
  
  rf_best_grid <- rf_hyper_grid %>% 
    dplyr::arrange(OOB_RMSE)
  
  # train best model
  best_rf_model <- ranger(
    formula         = y ~ ., 
    data            = dat, 
    num.trees       = rf_best_grid$n.trees[1],
    mtry            = rf_best_grid$mtry[1],
    min.node.size   = 5,
    seed            = 123
  )
  })
  models[["rf"]] <- best_rf_model
  runtimes[["rf"]] <- rf_time
  
  # Support Vector Machine
  svm_time <- system.time({
  svm_tune <- tune.svm(y ~ ., data = dat,
                        epsilon = seq(0.1, 0.5, 0.2),
                        cost = c(0.5, 1, 2)
  )
  
  svm_model <- svm_tune$best.model
  })
  
  models[["svm"]] <- svm_model
  runtimes[["svm"]] <- svm_time
  
  return(list(models, runtimes))
}



# This helper function takes in a model and a name for the model as parameters.
# It returns a data frame containing the coefficient estimates using that model
# as well as the names of the corresponding variables. This function is used in
# results_table().
model_data_frame <- function(model, model_name) {
  if (class(model) %in% c("cv.glmnet", "lm", "cv.ncvreg", "cv.gcdnet")){
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
    if (class(models[[i]])[1] %in% c("cv.glmnet", "lm", "cv.ncvreg", "cv.gcdnet")){
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
full_simulation <- function(seed, n, p, beta = NULL, ...) {
  set.seed(seed)
  
  # Set coefficient values. If beta is NULL, default values are used. Otherwise,
  # beta is extended/shortened to have length (p + 1).
  beta <- generate_coefficients(beta, p)
  
  # Generate training and test data.
  train_data <- generate_data(n = n, p = p, ...)
  test_data <- generate_data(n = n, p = p, ...)
  
  # Fit the models using the training data.
  models_list <- fit_models(train_data, n = n, p = p)
  models <- models_list[[1]]
  runtimes <- models_list[[2]]
  
  # Generate the coefficient table and compute the mean squared error
  # for each model.
  coefficients_table <- results_table(models, beta = beta, p = p)
  
  # Compute training and test MSE.
  train_mse <- lapply(models, mean_squared_error, dat = train_data)
  test_mse <- lapply(models, mean_squared_error, dat = test_data)
  
  # Generate confusion matrices for linear models.
  confusion_matrices <- confusion_matrices(coefficients_table)
  
  return(list(
    coefficients = coefficients_table, 
    #models = models, 
    train_mse = train_mse, 
    test_mse = test_mse, 
    confusion_matrices = confusion_matrices, runtimes = runtimes))
}



# This helper function repeats a full simulation until it successfully runs without
# any errors. This is used by monte_carlo() to ensure that the given number
# of iterations are run.
repeat_simulation_until_successful <- function(seed, n, p, beta = NULL, ...) {
  finished_simulation <- FALSE
  while (!finished_simulation) {
    tryCatch(
      {
        simulation_result <- full_simulation(seed = seed, n = n, p = p, beta = beta, ...)
        finished_simulation <- TRUE
      },
      error = function(error_message) {
        message("Error while running simulation. Another simulation will be run.")
        message(error_message)
      }
    )
  }

  return(simulation_result)
}



# This function iterates the monte_carlo_single_iteration() function multiple times.
# The seed is automatically set for each iteration. This function uses the parallel
# package to process multiple simulations at the same time.
#
# Arguments:
#   n: Number of observations in each iteration.
#   p: Number of predictors in each iteration.
#   iterations: Number of iterations to run
#   num_scores (default floor(detectCores() / 2)): Number of cores to use
#     to run simulations.
#   ...: Extra parameters used to generate data (see comments above
#     generate_data() for a list of optional parameters).
monte_carlo <- function(n, p, iterations,
                        num_cores = floor(detectCores() / 2), ...) {
  
  # Create the clusters using the parallel package.
  cl <- makeCluster(num_cores)
  
  # Load the needed libraries in each cluster.
  clusterEvalQ(cl, {
    library(tidyverse) # v1.3.1
    library(dplyr) # v1.0.6
    library(faux) # v1.0.0
    library(ncvreg) # v3.13.0
    library(glmnet) # v4.1-1
    library(gcdnet) #v1.0.5
    library(MASS) # v7.3-54
    library(caret) # v6.0-88
    library(xgboost) # v1.4.1.1
    library(ranger) # v0.12.1
    library(e1071) # v1.7-7
  })
  
  # We use this tryCatch to make sure that the clusters are closed, even if
  # an error stops the execution of this function.
  tryCatch({
    # Export all of the needed variables and functions to each cluster.
    clusterExport(cl, list("n", "p", "iterations", "...", "generate_coefficients",
           
                           
                                           "generate_data", "fit_models", "model_data_frame",
                           "results_table", "full_simulation",
                           "repeat_simulation_until_successful",
                           "mean_squared_error", "individual_confusion_matrix",
                           "confusion_matrices"),
                           envir = environment())
    
    # Run the simulations
    results <- parLapply(cl,
                         1:iterations, 
                         repeat_simulation_until_successful,
                         n = n,
                         p = p,
                         ...)
    },
    finally = {
      stopCluster(cl)
    })
  
  # Return the list of simulation results.
  return(results)
}