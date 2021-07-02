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

fit_models_test <- function(dat, n, p) {
  models <- list()
  runtimes <- list()
  
  # Null model for forward selection
  nm_time <- system.time(nm <- lm(y ~ 1, data = dat))
  models[["nm"]] <- nm
  runtimes[["nm"]] <- nm_time
  
  if (2 * p > n) {
    # Ridge model for dealing with multicollinearity
    set.seed(123)
    ridge_time <- system.time(ridge <- cv.glmnet(x = as.matrix(dat[,-1]),
                                                 y = dat$y, alpha = 0))
    models[["ridge"]] <- ridge
    runtimes[["ridge"]] <- ridge_time
  }
  
  return(list(models, runtimes))
}

original_results <- readRDS("../../results/monte-carlo/sim_results_50_100_3_independent_0.rds")

r <- lapply(c(13), function(i) {
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

  if (2 * p > n) {
    if (file.exists(original_filename)) {
      results <- readRDS(original_filename)
      for (i in 1:100) {
        set.seed(i)
        
        # Set coefficient values. If beta is NULL, default values are used. Otherwise,
        # beta is extended/shortened to have length (p + 1).
        beta <- generate_coefficients(NULL, p)
  
        # Generate training and test data.
        train_data <- generate_data(n = n, p = p, st_dev = st_dev, type = type, corr = corr)
        test_data <- generate_data(n = n, p = p, st_dev = st_dev, type = type, corr = corr)
  
        models_list <- fit_models_test(train_data, n = n, p = p)
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
      }
    }
  }
  
  saveRDS(results, file = save_filename)
  
  message("Finished running row ", i, " at ", Sys.time(), ". Time taken: ")
  
  return(results)
})

run_simulations_test(indices = 13, iterations = 100, num_cores = 1)
run_results <- readRDS("../../results/monte-carlo-test/sim_results_50_100_3_independent_0.rds")
repaired_results <- readRDS("../../results/monte-carlo-repair/sim_results_50_100_3_independent_0.rds")
