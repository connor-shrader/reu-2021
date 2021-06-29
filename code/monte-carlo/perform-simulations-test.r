rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("simulation-test.r")
source("metrics.r")

# Load parameters from file
load("../../data/monte-carlo/factorial-design.Rdata")

run_simulations <- function(indices, iterations = 1, ...) {
  results <- lapply(indices, function(i) {
    message("Beginning to run row ", i, ".")
    row <- parameters[i, ]
    n <- row$n
    p <- row$p
    st_dev <- row$sigma
    
    # Convert row$covar (which is a factor) to a character.
    type <- as.character(row$covar)
    corr <- row$rho
    
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

# res <- run_simulations(indices = 217:222, iterations = 100)













# for (i in 5:5) {
#   row <- parameters[i, ]
#   n <- row$n
#   p <- row$p
#   st_dev <- row$sigma
#   
#   # Convert row$covar (which is a factor) to a character.
#   type <- as.character(row$covar)
#   corr <- row$rho
#   
#   original_filename <- paste("../../results/monte-carlo/sim_results_",
#                     n, "_", 
#                     p, "_", 
#                     st_dev, "_", 
#                     type, "_", 
#                     corr, ".rds", sep = "")
#   
#   if (file.exists(original_filename)) {
#     results <<- readRDS(original_filename)
#     if (n == 200 & p == 100) {
#       for (i in 1:1) {
#         set.seed(i)
#         
#         # Set coefficient values. If beta is NULL, default values are used. Otherwise,
#         # beta is extended/shortened to have length (p + 1).
#         beta <- generate_coefficients(NULL, p)
#         
#         # Generate training and test data.
#         train_data <- generate_data(n = n, p = p, st_dev = st_dev, type = type, corr = corr)
#         test_data <- generate_data(n = n, p = p, st_dev = st_dev, type = type, corr = corr)
#         
#         models <- list()
#         runtimes <- list()
#         
#         # Full model
#         fm_time <- system.time(fm <- lm(y ~ ., data = train_data))
#         models[["fm"]] <- fm
#         runtimes[["fm"]] <- fm_time
#         
#         # Null model for forward selection
#         nm_time <- system.time(nm <- lm(y ~ 1, data = train_data))
# 
#         
#         # AIC and BIC model selection for forward
#         af_time <- system.time(af <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
#                                               direction="forward", k=2, trace=F,
#                                               steps=3000)) #AIC
#         models[["af"]] <- af
#         runtimes[["af"]] <- af_time
#         
#         bf_time <- system.time(bf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
#                                               direction="forward", k=log(nrow(train_data)),
#                                               trace=F, steps=3000)) #BIC
#         models[["bf"]] <- bf
#         runtimes[["bf"]] <- bf_time
#         
#         # AIC and BIC model selection for stepwise forward
#         asf_time <- system.time(asf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
#                                                 direction="both", k=2, trace=F,
#                                                 steps=3000)) #AIC
#         models[["asf"]] <- asf
#         runtimes[["asf"]] <- asf_time
#         
#         bsf_time <- system.time(bsf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
#                                                 direction="both", k=log(nrow(train_data)),
#                                                 trace=F, steps=3000)) #BIC  
#         models[["bsf"]] <- bsf
#         runtimes[["bsf"]] <- bsf_time
#         
#         ridge_time <- system.time(ridge <- cv.glmnet(x = as.matrix(train_data[,-1]),
#                                                      y = train_data$y, alpha = 0))
#         models[["ridge"]] <- ridge
#         runtimes[["ridge"]] <- ridge_time
#         
#         # Generate the coefficient table and compute the mean squared error
#         # for each model.
#         coefficients_table2 <- results_table(models, beta = beta, p = p)
#         coefficients_table2_no_soln <- results_table(models, beta = beta, p = p)[, -1]
#         
#         # Compute training and test MSE.
#         train_mse2 <- lapply(models, mean_squared_error, dat = train_data)
#         test_mse2 <- lapply(models, mean_squared_error, dat = test_data)
#         
#         results[[i]]$coefficients <- cbind(results[[i]]$coefficients, coefficients_table2_no_soln)
#         results[[i]]$train_mse <- c(results[[i]]$train_mse, train_mse2)
#         results[[i]]$test_mse <- c(results[[i]]$test_mse, test_mse2)
#         results[[i]]$runtime <- c(results[[i]]$runtimes, runtimes)
#         results[[i]]$confusion_matrices <- c(results[[i]]$confusion_matrices, confusion_matrices(coefficients_table2)[-1])
#         
#         names <- c("nm", "fm", "af", "bf", "asf", "bsf", "ridge", "lasso", "enet", "adap_ridge", "adap_lasso", "adap_enet", "scad", "mcp")
#         
#         #results[[i]]$coefficients <- results[[i]]$coefficients[c("soln", names)]
#         #results[[i]]$train_mse <- results[[i]]$train_mse[names]
#         #results[[i]]$test_mse <- results[[i]]$test_mse[names]
#       }
#     }
#   }
# }