# aggregate-results.r

rm(list = ls())
library(rstudioapi) # v0.13

# Needed for rbind.fill when combining rows in the aggregate results dataframe.
library(plyr)

setwd(dirname(getActiveDocumentContext()$path))


get_results_file <- function(n, p, st_dev, type, corr) {
  print("In func")
  filename <- paste("../../results/monte-carlo/sim_results_",
                    n, "_", 
                    p, "_", 
                    st_dev, "_", 
                    type, "_", 
                    corr, ".rds", sep = "")
  
  if(file.exists(filename)) {
    return(readRDS(filename))
  } else {
    return(NULL)
  }
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

compute_results <- function(n, p, st_dev, type, corr) {
  results <- get_results_file(n, p, st_dev, type, corr)
  mean_mses <- get_mean_mse(results)
  # variable_selections <- get_variable_selections(results)
  
  return(c(list(n = n, p = p, st_dev = st_dev, type = type, corr = corr),
           mean_mses))
}

aggregate_results <- function(indices = 1:270) {
  load("../../data/monte-carlo/factorial-design.Rdata")
  
  all_results <- lapply(indices, function(i) {
    row <- parameters[i, ]
    
    n <- row$n
    p <- row$p
    st_dev <- row$sigma
    
    # Convert row$covar (which is a factor) to a character.
    type <- as.character(row$covar)
    corr <- row$rho
    
    return(as.data.frame(compute_results(n, p, st_dev, type, corr)))
  })
  
  all_results_df <- as.data.frame(do.call(rbind.fill, all_results))
  return(all_results_df)
}

all_res <- aggregate_results(1:8)
