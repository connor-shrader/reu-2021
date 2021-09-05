# aggregate-results.r

# This file aggregates the results from the 270 results files into single tables.

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path)) # set environment context

# Needed for rbind.fill when combining rows in the aggregate results dataframe.
library(plyr) # v1.8.6

# Get parameters file
parameters <- readRDS("../../data/monte-carlo/factorial-design.rds")

# This function reads the file corresponding to the given values for n, p, etc.
# It then returns the R object stored in that file. If the file does not exist,
# this function returns NULL.
get_results <- function(response, n, p, st_dev, type, corr) {
  if (response == 1) {
    filename <- "../../results/monte-carlo-linear"
  }
  else if (response == 2) {
    filename <- "../../results/monte-carlo-nonlinear"
  }
  else {
    stop("Invalid response given: Use 1 for a linear relationship or 2 for 
          a non-linear relationship")
  }
  
  filename <- paste(filename, "/sim_results_",
                    response, "_",
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

# This lapply iterates through all possible parameter combinations and returns
# a list of data frames. Each data frame contains results for the 100 simulations
# for a single parameter combination.
for (response in 1:2) {
  all_results <- lapply(1:270, function(i) {
    #print(i)
    row <- parameters[i, ]
    
    n <- row$n
    p <- row$p
    st_dev <- row$st_dev
    type <- as.character(row$type) #row$type is a factor
    corr <- row$corr
    
    results <- get_results(response, n, p, st_dev, type, corr)
    
    if (is.null(results)) {
      return(data.frame())
    }
    
    # This lapply iterates through all 100 simulations for one parameter combination
    # (i.e. one row) and collects all of the metrics into a list of data frames.
    metrics_for_one_row <- lapply(1:100, function(iteration) {
      #print(iteration)
      model_names <- names(results[[iteration]]$test_mse)
      
      # The following lapply statement iterations through all models from a single
      # simulation run and returns a list of data frames, each with one row. This
      # row contains the values of n, p, st_dev, type, corr, model_name, as well as
      # the train mse, test mse, true negatives, false negatives, false positives
      # true positives, and elapsed time.
      metrics_for_one_simulation <- lapply(model_names, function(model_name) {
        simulation <- results[[iteration]]
        
        simulation_summary <- list(row_index = i,
                                   n = n,
                                   p = p, 
                                   st_dev = st_dev, 
                                   type = type, 
                                   corr = corr, 
                                   model_name = model_name,
                                   train_mse = simulation$train_mse[[model_name]],
                                   test_mse = simulation$test_mse[[model_name]])
        
        if (model_name %in% names(simulation$confusion_matrices)) {
          #print(model_name)
          simulation_summary <- c(simulation_summary, list(
            tn = simulation$confusion_matrices[[model_name]]$table[1, 1],
            fn = simulation$confusion_matrices[[model_name]]$table[1, 2],
            fp = simulation$confusion_matrices[[model_name]]$table[2, 1],
            tp = simulation$confusion_matrices[[model_name]]$table[2, 2]))
        }
        
        simulation_summary <- c(simulation_summary, list(runtime = simulation$runtimes[[model_name]][["elapsed"]]))
        
        return(data.frame(simulation_summary))
      })
      
      #print(test_mses_for_one_simulation)
      
      # Combine all of the rows from the previous lapply into a single data frame.
      # Each row corresponds to one model, and the whole data frame corresponds to one
      # combination of n/p/st_dev/type/corr.
      return(do.call(rbind.fill, metrics_for_one_simulation))
    })
    
    # Combine the data frames for all 100 simulations into a single data frame.
    return(do.call(rbind.fill, metrics_for_one_row))
  })
  
  # Combine all of the results from the previous lapply into one massive data frame.
  all_results <- do.call(rbind.fill, all_results)
  
  # Remove the unnecessary "null model" and "solution model" rows.
  all_results <- all_results[all_results$model_name != "nm" & 
                               all_results$model_name != "soln" , ]
  
  all_results$sensitivity <- all_results$tp / (all_results$tp + all_results$fn)
  all_results$specificity <- all_results$tn / (all_results$tn + all_results$fp)
  
  # Set several of the columns into factors (instead of numeric)
  all_results$row_index <- factor(all_results$row_index)
  all_results$n <- factor(all_results$n)
  all_results$p <- factor(all_results$p)
  all_results$st_dev <- factor(all_results$st_dev)
  all_results$type <- factor(all_results$type, 
                             levels = c("independent", "symmetric",
                                        "autoregressive", "blockwise"))
  all_results$corr <- factor(all_results$corr)
  all_results$model_name <- factor(all_results$model_name, 
                                   levels = c("fm", "ab", "bb", "asb", "bsb",
                                              "af", "bf", "asf", "bsf", "ridge",
                                              "lasso", "enet", "adap_ridge",
                                              "adap_lasso", "adap_enet", "scad",
                                              "mcp", "gbm", "rf", "svm"))
  
  # mean_results is a data frame that contains the mean values for test_mse, train_mse,
  # true negative, false negative, false positive, and true positive for the 100
  # simulations for every combination of n/p/st_dev/type/corr/model_name.
  mean_results <- aggregate(list(mean_test_mse = all_results$test_mse,
                                 mean_train_mse = all_results$train_mse,
                                 mean_tn = all_results$tn,
                                 mean_fn = all_results$fn,
                                 mean_fp = all_results$fp,
                                 mean_tp = all_results$tp,
                                 mean_sensitivity = all_results$sensitivity,
                                 mean_specificity = all_results$specificity),
                            by = list(row_index = all_results$row_index,
                                      n = all_results$n,
                                      p = all_results$p,
                                      st_dev = all_results$st_dev,
                                      type = all_results$type,
                                      corr = all_results$corr,
                                      model_name = all_results$model_name),
                            FUN = mean)
  
  # This table is the same as mean_results but with the standard deviation instead
  # of the mean.
  sd_results <- aggregate(list(sd_test_mse = all_results$test_mse,
                               sd_train_mse = all_results$train_mse,
                               sd_tn = all_results$tn,
                               sd_fn = all_results$fn,
                               sd_fp = all_results$fp,
                               sd_tp = all_results$tp,
                               sd_sensitivity = all_results$sensitivity,
                               sd_specificity = all_results$specificity),
                          by = list(row_index = all_results$row_index,
                                    n = all_results$n,
                                    p = all_results$p,
                                    st_dev = all_results$st_dev,
                                    type = all_results$type,
                                    corr = all_results$corr,
                                    model_name = all_results$model_name),
                          FUN = sd)
  
  # Combine mean_results and sd_results into a single data frame.
  aggregate_results <- merge(mean_results, 
                             sd_results, 
                             by = c("row_index", "n", "p", "st_dev",
                                    "type", "corr", "model_name"))
  
  if (response == 1) {
    saveRDS(all_results, file = "../../results/monte-carlo-linear/all_linear_results.rds")
    saveRDS(aggregate_results, file = "../../results/monte-carlo-linear/aggregate_linear_results.rds")
  }
  else if (response == 2) {
    saveRDS(all_results, file = "../../results/monte-carlo-nonlinear/all_nonlinear_results.rds")
    saveRDS(aggregate_results, file = "../../results/monte-carlo-nonlinear/aggregate_nonlinear_results.rds")
  }
}