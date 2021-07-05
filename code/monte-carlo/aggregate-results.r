# aggregate-results.r

# This file aggregates the results from the 270 results files into single tables.

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))

# Needed for rbind.fill when combining rows in the aggregate results dataframe.
library(plyr)



# Get parameters file
load("../../data/monte-carlo/factorial-design.Rdata")

get_results <- function(n, p, st_dev, type, corr) {
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

all_results <- lapply(1:270, function(i) {
  #print(i)
  row <- parameters[i, ]
  
  n <- row$n
  p <- row$p
  st_dev <- row$st_dev
  type <- as.character(row$type) #row$type is a factor
  corr <- row$corr
  
  results <- get_results(n, p, st_dev, type, corr)
  
  test_mse_for_one_row <- lapply(1:100, function(iteration) {
    #print(iteration)
    model_names <- names(results[[iteration]]$test_mse)
    
    test_mses_for_one_simulation <- lapply(model_names, function(model_name) {
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
    
    return(do.call(rbind.fill, test_mses_for_one_simulation))
  })
  
  return(do.call(rbind.fill, test_mse_for_one_row))
})


all_results <- do.call(rbind.fill, all_results)
all_results <- all_results[all_results$model_name != "nm" & 
                             all_results$model_name != "soln" , ]

mean_results <- aggregate(list(mean_test_mse = all_results$test_mse,
                               mean_train_mse = all_results$train_mse,
                               mean_tn = all_results$tn,
                               mean_fn = all_results$fn,
                               mean_fp = all_results$fp,
                               mean_tp = all_results$tp),
                          by = list(row_index = all_results$row_index,
                                    n = all_results$n,
                                    p = all_results$p,
                                    st_dev = all_results$st_dev,
                                    type = all_results$type,
                                    corr = all_results$corr,
                                    model_name = all_results$model_name),
                          FUN = mean)

sd_results <- aggregate(list(sd_test_mse = all_results$test_mse,
                             sd_train_mse = all_results$train_mse,
                             sd_tn = all_results$tn,
                             sd_fn = all_results$fn,
                             sd_fp = all_results$fp,
                             sd_tp = all_results$tp),
                        by = list(row_index = all_results$row_index,
                                  n = all_results$n,
                                  p = all_results$p,
                                  st_dev = all_results$st_dev,
                                  type = all_results$type,
                                  corr = all_results$corr,
                                  model_name = all_results$model_name),
                        FUN = sd)

aggregate_results <- merge(mean_results, sd_results, by = c("row_index", "n", "p", "st_dev", "type", "corr", "model_name"))

correct_order <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf", "bsf",
                   "ridge", "lasso", "enet", "adap_ridge", "adap_lasso", 
                   "adap_enet", "scad", "mcp", "gbm", "rf", "svm")
aggregate_results$model_name <- factor(aggregate_results$model_name, levels = correct_order)

saveRDS(all_results, file = "../../results/monte-carlo/all_results.rds")
saveRDS(aggregate_results, file = "../../results/monte-carlo/aggregate_results.rds")