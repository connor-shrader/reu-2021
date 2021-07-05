rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("simulation.r")
source("metrics.r")

# Load parameters from file
load("../../data/monte-carlo/factorial-design.Rdata")

run_simulations_test <- function(indices, iterations = 1, ...) {
  results <- lapply(indices, function(i) {
    message("Beginning to run row ", i, ".")
    row <- parameters[i, ]
    n <- row$n
    p <- row$p
    st_dev <- row$st_dev
    
    # Convert row$covar (which is a factor) to a character.
    type <- as.character(row$type)
    corr <- row$corr
    
    filename <- paste("../../results/monte-carlo-test/sim_results_",
                      n, "_", 
                      p, "_", 
                      st_dev, "_", 
                      type, "_", 
                      corr, ".rds", sep = "")
    
    block_size <- 0
    if (!file.exists(filename)) {
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
      
      time_taken <- system.time(results <- monte_carlo(n = n,
                                                       p = p,
                                                       iterations = iterations,
                                                       st_dev = st_dev,
                                                       type = type,
                                                       corr = corr,
                                                       block_size = block_size,
                                                       ...))
      
      message("Finished running row ", i, " at ", Sys.time(), ". Time taken: ")
      print(time_taken)
      
      saveRDS(results, file = filename)
    }
    else {
      message("Results file already exists for row ", i, ".")
    }
  })
  
  return(results)
}

fit_ridge <- function(dat, n, p) {
  models <- list()
  runtimes <- list()
  
  if (2 * p > n) {
    # Ridge model for dealing with multicollinearity
    set.seed(123)
    ridge_time <- system.time(ridge <- cv.glmnet(x = as.matrix(dat[,-1]),
                                                 y = dat$y, alpha = 0))
    models[["ridge"]] <- ridge
    runtimes[["ridge"]] <- ridge_time
    
    #print(ridge_time)
  }
  
  return(list(models, runtimes))
}



num_cores = floor(detectCores() / 2)
  
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

# Export all of the needed variables and functions to each cluster.
clusterExport(cl, list("fit_ridge", "run_simulations_test", "parameters",
                       "generate_coefficients",
                       "generate_data", "fit_models", "model_data_frame",
                       "results_table", "full_simulation",
                       "repeat_simulation_until_successful",
                       "mean_squared_error", "individual_confusion_matrix",
                       "confusion_matrices"),
              envir = environment())

parLapply(cl, 1:9, function(i) {
  row <- parameters[i, ]
  n <- row$n
  p <- row$p
  st_dev <- row$st_dev

  # Convert row$corr (which is a factor) to a character.
  type <- as.character(row$type)
  corr <- row$corr

  original_filename <- paste("../../results/monte-carlo/sim_results_",
                    n, "_",
                    p, "_",
                    st_dev, "_",
                    type, "_",
                    corr, ".rds", sep = "")
  
  save_filename <- paste("../../results/monte-carlo-repair/sim_results_",
                             n, "_",
                             p, "_",
                             st_dev, "_",
                             type, "_",
                             corr, ".rds", sep = "")

  results <- NA
  if (2 * p > n & file.exists(original_filename)) {
    message("About to run ridge for row ", i)
    results <- readRDS(original_filename)
    for (i in 1:100) {
      set.seed(i)
      
      # Set coefficient values. If beta is NULL, default values are used. Otherwise,
      # beta is extended/shortened to have length (p + 1).
      beta <- generate_coefficients(NULL, p)

      # Generate training and test data.
      system.time(train_data <- generate_data(n = n, p = p, st_dev = st_dev, type = type, corr = corr))
      test_data <- generate_data(n = n, p = p, st_dev = st_dev, type = type, corr = corr)
      
      models_list <- fit_ridge(train_data, n = n, p = p)
      models <- models_list[[1]]
      runtimes <- models_list[[2]]
      
      # Generate the coefficient table and compute the mean squared error
      # for each model.
      
      coefficients_table2 <- results_table(models, beta = beta, p = p)
      confusion_matrices2 <- confusion_matrices(coefficients_table2)
      
     #print(models)
      # Compute training and test MSE.
      train_mse2 <- lapply(models, mean_squared_error, dat = train_data)
      test_mse2 <- lapply(models, mean_squared_error, dat = test_data)
      
      results[[i]]$coefficients$ridge <- coefficients_table2$ridge
      results[[i]]$train_mse$ridge <- train_mse2$ridge
      results[[i]]$test_mse$ridge <- test_mse2$ridge
      results[[i]]$confusion_matrices$ridge <- confusion_matrices2$ridge
      results[[i]]$runtimes$ridge <- runtimes$ridge

      names <- c("nm", "ridge", "lasso", "enet", "adap_ridge", "adap_lasso", "adap_enet", "scad", "mcp")
      #print(results[[i]]$train_mse)
      results[[i]]$coefficients <- results[[i]]$coefficients[, c("soln", names)]
      results[[i]]$train_mse <- results[[i]]$train_mse[c(names, "gbm", "rf", "svm")]
      results[[i]]$test_mse <- results[[i]]$test_mse[c(names, "gbm", "rf", "svm")]
      results[[i]]$runtimes <- results[[i]]$runtimes[c(names, "gbm", "rf", "svm")]
      results[[i]]$confusion_matrices <- results[[i]]$confusion_matrices[c("soln", names)]
      
      message("Finished iteration ", i)
    }
    
    saveRDS(results, file = save_filename)
    
    message("Finished running row ", i, " at ", Sys.time())
  }
  
  return(results)
})

stopCluster(cl)

#run_simulations_test(indices = 1:9, iterations = 3)

#original_results <- readRDS("../../results/monte-carlo/sim_results_50_100_3_independent_0.rds")
