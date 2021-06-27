rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))


get_results_file <- function(n, p, st_dev, type, corr) {
  filename <- paste("../../results/monte-carlo/sim_results_",
                    n, "_", 
                    p, "_", 
                    st_dev, "_", 
                    type, "_", 
                    corr, ".rds", sep = "")
  
  return(readRDS(filename))
}

get_mean_mse <- function(results) {
  train_mses <- lapply(results, function(iteration) {
    return(unlist(iteration$train_mse))
  })
  test_mses <- lapply(results, function(iteration) {
    return(unlist(iteration$test_mse))
  })
  
  p <- length(test_mses)
  
  ave_train_mses <- (1 / p) * Reduce("+", train_mses)
  ave_test_mses <- (1 / p) * Reduce("+", test_mses)
  names(ave_train_mses) <- paste("mean_train_",
                                 names(ave_train_mses),
                                 "_mse",
                                 sep = "")
  names(ave_test_mses) <- paste("mean_test_",
                                names(ave_test_mses),
                                "_mse",
                                sep = "")
  
  return(c(as.list(ave_train_mses), as.list(ave_test_mses)))
}

get_variable_selections <- function(results) {
  penalized_model_names <- c("nm", "fm", "ab", "bb", "asb", "bsb", "af", "bf",
                             "asf", "bsf", "ridge", "lasso", "enet",
                             "adap_ridge", "adap_lasso", "adap_enet",
                             "scad", "mcp")
  
  matrices <- lapply(results, function(iteration) {
    return(iteration$confusion_matrices)
  })
  
  avg_confusion_matrices <- lapply(penalized_model_names, function(model) {
    p <- length(matrices)
    
    matrix_list <- lapply(1:p, function(iteration) {
      return(matrices[[iteration]][[model]]$table)
    })
    
    avg_matrix <- (1 / p) * Reduce("+", matrix_list)
    return(avg_matrix)
  })
  
  names(avg_confusion_matrices) <- penalized_model_names
  return(avg_confusion_matrices)
}


res <- get_results_file(n = 50, p = 10, st_dev = 1, type = "independent", corr = 0)
mean_mses <- get_mean_mse(res)
mat <- get_variable_selections(res)

names(res[[1]]$confusion_matrices)
