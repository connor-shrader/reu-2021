# aggregate-results.r

rm(list = ls())
library(rstudioapi) # v0.13

# Needed for rbind.fill when combining rows in the aggregate results dataframe.
library(plyr)

setwd(dirname(getActiveDocumentContext()$path))


get_results_file <- function(n, p, st_dev, type, corr) {
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

# get_mean_mse <- function(results) {
#   train_mses <- lapply(results, function(iteration) {
#     return(unlist(iteration$train_mse))
#   })
#   test_mses <- lapply(results, function(iteration) {
#     return(unlist(iteration$test_mse))
#   })
#   
#   p <- length(test_mses)
#   
#   ave_train_mses <- (1 / p) * Reduce("+", train_mses)
#   ave_test_mses <- (1 / p) * Reduce("+", test_mses)
#   names(ave_train_mses) <- paste("mean_train_",
#                                  names(ave_train_mses),
#                                  "_mse",
#                                  sep = "")
#   names(ave_test_mses) <- paste("mean_test_",
#                                 names(ave_test_mses),
#                                 "_mse",
#                                 sep = "")
#   
#   return(c(as.list(ave_train_mses), as.list(ave_test_mses)))
# }
# 
# get_variable_selections <- function(results) {
#   penalized_model_names <- c("nm", "fm", "ab", "bb", "asb", "bsb", "af", "bf",
#                              "asf", "bsf", "ridge", "lasso", "enet",
#                              "adap_ridge", "adap_lasso", "adap_enet",
#                              "scad", "mcp")
#   
#   matrices <- lapply(results, function(iteration) {
#     return(iteration$confusion_matrices)
#   })
#   
#   avg_confusion_matrices <- lapply(penalized_model_names, function(model) {
#     p <- length(matrices)
#     
#     matrix_list <- lapply(1:p, function(iteration) {
#       return(matrices[[iteration]][[model]]$table)
#     })
#     
#     avg_matrix <- (1 / p) * Reduce("+", matrix_list)
#     return(avg_matrix)
#   })
#   
#   names(avg_confusion_matrices) <- penalized_model_names
#   return(avg_confusion_matrices)
# }
# 
# compute_results <- function(n, p, st_dev, type, corr) {
#   results <- get_results_file(n, p, st_dev, type, corr)
#   mean_mses <- get_mean_mse(results)
#   # variable_selections <- get_variable_selections(results)
#   
#   return(c(list(n = n, p = p, st_dev = st_dev, type = type, corr = corr),
#            mean_mses))
# }
# 
# aggregate_results <- function(indices = 1:270) {
#   load("../../data/monte-carlo/factorial-design.Rdata")
#   
#   all_results <<- lapply(indices, function(i) {
#     row <- parameters[i, ]
#     
#     n <- row$n
#     p <- row$p
#     st_dev <- row$sigma
#     
#     # Convert row$covar (which is a factor) to a character.
#     type <- as.character(row$covar)
#     corr <- row$rho
#     
#     return(as.data.frame(compute_results(n, p, st_dev, type, corr)))
#   })
#   
#   all_results_df <<- as.data.frame(do.call(rbind.fill, all_results))
#   return(all_results_df)
# }





# get_confusion_matrix_entries <- function(iteration, i = 1, j = 1) {
#   metric_by_model <- lapply(iteration$confusion_matrices, function(matrix) {
#     matrix$table[i, j]
#   })
# 
#   return(unlist(metric_by_model[-1]))
# }
# 
# aggregate_average_metric <- function(fun, indices = 1:270, ...) {
#   metric_averages <- lapply(indices, function(i) {
#     row <- parameters[i, ]
# 
#     n <- row$n
#     p <- row$p
#     st_dev <- row$sigma
#     type <- as.character(row$covar) #row$covar is a factor
#     corr <- row$rho
# 
#     results <- get_results_file(n, p, st_dev, type, corr)
# 
#     metric_per_simulation <- lapply(results, fun, ...)
# 
#     average_metric <- (1 / length(metric_per_simulation)) * Reduce("+", metric_per_simulation)
# 
#     parameter_values <- c(n, p, st_dev, type, corr)
#     names(parameter_values) <- c("n", "p", "st_dev", "type", "corr")
#     return(c(parameter_values, average_metric))
#   })
# 
#   metric_averages <- lapply(metric_averages, function(row) {data.frame(t(row))})
# 
#   return(do.call(rbind.fill, metric_averages))
# }
# 
# tn <- aggregate_average_metric(fun = get_confusion_matrix_entries, indices = 1:3, i = 1, j = 1)
# 
# train_mse <- aggregate_average_metric(fun = function(iteration) unlist(iteration$train_mse), indices = 1:3)





load("../../data/monte-carlo/factorial-design.Rdata")

# x <- lapply(1:3, function(i) {
#   row <- parameters[i, ]
# 
#   n <- row$n
#   p <- row$p
#   st_dev <- row$sigma
#   type <- as.character(row$covar) #row$covar is a factor
#   corr <- row$rho
# 
#   results <- get_results_file(n, p, st_dev, type, corr)
#   
#   test_mse <- lapply(1:100, function(iteration) {
#     test_mse_for_one_simulation <- results[[iteration]]$test_mse
#     return(data.frame(c(list(n = n, p = p, st_dev = st_dev, type = type, corr = corr), test_mse_for_one_simulation)))
#   })
# })

all_results <- lapply(1:3, function(i) {
  #print(i)
  row <- parameters[i, ]

  n <- row$n
  p <- row$p
  st_dev <- row$sigma
  type <- as.character(row$covar) #row$covar is a factor
  corr <- row$rho

  results <- get_results_file(n, p, st_dev, type, corr)

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

correct_order <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf", "bsf",
                   "ridge", "lasso", "enet", "adap_ridge", "adap_lasso", 
                   "adap_enet", "scad", "mcp", "gbm", "rf", "svm")

#mean_results$model_name <- factor(mean_results$model_name, levels = correct_order)
#mean_results <- mean_results[order(mean_results$model_name), ]

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

#sd_results$model_name <- factor(sd_results$model_name, levels = correct_order)
#sd_results <- sd_results[order(sd_results$model_name), ]
#sd_results <- subset(sd_results, select = -row_index)

aggregate_results <- merge(mean_results, sd_results, by = c("row_index", "n", "p", "st_dev", "type", "corr", "model_name"))

aggregate_results$model_name <- factor(aggregate_results$model_name, levels = correct_order)
#aggregate_results <- aggregate_results[order(aggregate_results$model_name), ]

# agg.mean <- aggregate(test_mse ~ model_name + n, data = x, FUN = mean)
# agg.sd <- aggregate(test_mse ~ model_name + n, data = x, FUN = sd)
# agg <- merge(agg.mean, agg.sd, by = c("model_name", "n"))
# names(agg) <- c("model_name", "n", "mean", "sd")
# 
library(ggplot2)
# 
# ggplot(data = agg, mapping = aes(x = model_name)) +
#   geom_col(data = agg, mapping = aes(x = model_name, y = mean), fill = "red") + 
#   geom_errorbar(data = agg, mapping = aes(x = model_name, y = mean, ymin = mean - sd, ymax = mean + sd)) +
#   facet_grid(rows = vars(n))

specific_results <- aggregate_results[aggregate_results$p == 10 & 
                                        aggregate_results$st_dev == 1 &
                                        aggregate_results$type == "independent" &
                                        aggregate_results$corr == 0, ]

x <- aggregate_results[aggregate_results$p == 10]

ggplot(data = specific_results) +
  geom_col(mapping = aes(x = model_name, y = mean_test_mse), fill = "red") +
  geom_errorbar(mapping = aes(x = model_name, y = mean_test_mse, ymin = mean_test_mse - sd_test_mse, ymax = mean_test_mse + sd_test_mse)) +
  facet_grid(rows = vars(n))


plot_metric <- function(data, metric, facet, ...) {
  args <- list(...)
  
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  for (fixed_parameter in names(args)) {
    data <- data[data[[fixed_parameter]] == args[[fixed_parameter]], ]
  }

  ggplot(data = data) +
    geom_col(mapping = aes_string(x = "model_name", y = mean_metric), fill = "red") + 
    geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    facet_grid(reformulate(".", facet))
}
