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

x <- lapply(1:3, function(i) {
  row <- parameters[i, ]

  n <- row$n
  p <- row$p
  st_dev <- row$sigma
  type <- as.character(row$covar) #row$covar is a factor
  corr <- row$rho

  results <- get_results_file(n, p, st_dev, type, corr)

  test_mse_for_one_row <- lapply(1:100, function(iteration) {
    model_names <- names(results[[iteration]]$test_mse)
    #print("1")
    test_mses_for_one_simulation <- lapply(model_names, function(model_name) {
      #print("2")
      #print(results[[iteration]]$test_mse)
      #print(model_name)
      data.frame(list(n = n, p = p, st_dev = st_dev, type = type, corr = corr, model_name = model_name, test_mse = results[[iteration]]$test_mse[[model_name]]))
    })
    
    return(do.call(rbind.fill, test_mses_for_one_simulation))
  })
  
  return(do.call(rbind.fill, test_mse_for_one_row))
})


x <- do.call(rbind.fill, x)
x <- x[x$model_name != "nm", ]



agg.mean <- aggregate(test_mse ~ model_name + n, data = x, FUN = mean)
agg.sd <- aggregate(test_mse ~ model_name + n, data = x, FUN = sd)
agg <- merge(agg.mean, agg.sd, by = c("model_name", "n"))
names(agg) <- c("model_name", "n", "mean", "sd")

library(ggplot2)

ggplot(data = agg, mapping = aes(x = model_name)) +
  geom_col(data = agg, mapping = aes(x = model_name, y = mean), fill = "red") + 
  geom_errorbar(data = agg, mapping = aes(x = model_name, y = mean, ymin = mean - sd, ymax = mean + sd)) +
  facet_grid(rows = vars(n))
