# test-plots.r
# Gabe Ackall, Connor Shrader

# This file is used to experiment with plotting results

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")
source("../data-analysis/subset-data.r")

# Used to create plots
library(ggplot2) # v3.3.3

# plyr is used to map the values of the model_names column to more readable
# values.
library(plyr) # v1.8.6

# Used to get the default color/fill scales for ggplot2.
library(scales) # v1.1.1

library(caret)

# This function takes in a data frame, a string representing a metric, and
# a column to use as a facet. This function then returns a facet grid of
# the data using ggplot2.
plot_metric_old <- function(data, metric, facet, ...) {
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  data <- subset_data(data, ...)
  
  ggplot(data = data) +
    geom_col(mapping = aes_string(x = "model_name", y = mean_metric), fill = "red") + 
    geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    facet_grid(reformulate(".", facet)) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}

# This function is a more complete version of plot_metric.
plot_metric <- function(data, metric, facet, color, ylabel = "Mean test MSE", 
                        fixy = FALSE, large_text = FALSE, ...) {
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  dat <- subset_data(data, ...)
  
  plt <- ggplot(data = dat) +
    geom_point(mapping = aes_string(x = "model_name", y = mean_metric, color = color, shape = color, fill = color), size = 4) + 
    # geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    scale_shape_manual(values = 21:24, name = "Correlation") +
    scale_color_manual(values = hue_pal()(4), name = "Correlation") +
    scale_fill_manual(values = hue_pal()(4), name = "Correlation") +
    labs(x = "Model", y = ylabel, color = "Correlation", shape = "Correlation") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.2),
      panel.grid = element_line(color = "gray90"),
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    )
  
  if(large_text == TRUE) {
    plt <- plt + theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 18),
      axis.text.y = element_text(size = 18),
      strip.text = element_text(size = 18),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      legend.position="bottom",
      legend.box = "horizontal"
    )
  }
  
  # If fixy is false, then let y vary for different values of sigma. Otherwise, keep
  # the y scale the same
  if (fixy == FALSE) {
    plt <- plt + facet_grid(reformulate(facet[1], facet[2]), scales = "free_y", label = "label_parsed")
  }
  else {
    plt <- plt + facet_grid(reformulate(facet[1], facet[2]), label = "label_parsed")
  }
  
  return(plt)
}

for (response in 1:2) {
  if (response == 1) {
    aggregate_results <- readRDS("../../results/monte-carlo-linear/aggregate_linear_results.rds")
    all_results <- readRDS("../../results/monte-carlo-linear/all_linear_results.rds")
    directory <- "../../figures/linear-facet/"
  }
  if (response == 2) {
    aggregate_results <- readRDS("../../results/monte-carlo-nonlinear/aggregate_nonlinear_results.rds")
    all_results <- readRDS("../../results/monte-carlo-nonlinear/all_nonlinear_results.rds")
    directory <- "../../figures/nonlinear-facet/"
  }
  
  aggregate_results <- aggregate_results[
    !aggregate_results$model_name %in% c("adap_ridge", "adap_lasso", "adap_enet"), ]
  
  all_results <- all_results[
    !all_results$model_name %in% c("adap_ridge", "adap_lasso", "adap_enet"), ]
  
  # aggregate_results <- aggregate_results[aggregate_results$model_name != "gbm" &
  #                                       aggregate_results$model_name != "rf" &
  #                                       aggregate_results$model_name != "svm", ]
  
  old_names <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
                 "bsf", "ridge", "lasso", "enet", "scad", "mcp", "gbm",
                 "rf", "svm")
  new_names <- c("OLS", "AIC B", "BIC B", "AIC SB",
                 "BIC SB", "AIC F", "BIC F",
                 "AIC SF", "BIC SF", "Ridge", "Lasso",
                 "E-net", "SCAD", "MCP", "XGBoost", "RF", "SVM")
  
  # Replace model names with more readable names.
  aggregate_results$model_name <- mapvalues(aggregate_results$model_name,
                                            from = old_names, to = new_names)
  
  all_results$model_name <- mapvalues(all_results$model_name,
                                      from = old_names, to = new_names)
  
  # Below is some old code used to generate a table to put into the final report.
  # I ended up deciding to use the tabular() function from the tables library
  # instead.
  
  # sub_results <- subset_data(aggregate_results, p = 100, n = 200)[c("st_dev", "type", "corr", "model_name", "mean_test_mse", "sd_test_mse")]
  # x <- melt(sub_results, id = c("type", "corr", "model_name", "st_dev"), measured = c("mean_test_mse", "sd_test_mse"))
  # y <- cast(x, st_dev + model_name ~ type + corr)
  
  plot_results <- aggregate_results
  
  # Rename the st_dev column so that the plot has better facet labels.
  plot_results$st_dev <- mapvalues(plot_results$st_dev,
                                   from = c("1", "3", "6"),
                                   to = c("sigma == 1", "sigma == 3", "sigma == 6"))
  
  # Capitalize the type column
  plot_results$type <- mapvalues(plot_results$type,
                                 from = c("independent", "symmetric",
                                          "autoregressive", "blockwise"),
                                 to = c("Independent", "Symmetric",
                                        "Autoregressive", "Blockwise"))
  
  plot_results <- arrange(plot_results, corr)
  accuracy_results <- plot_results[!plot_results$model_name %in% c("OLS",
                                   "Ridge", "Adap. ridge", "XGBoost", "RF", "SVM"), ]
  
  dimensions <- expand.grid(n = c(50, 200, 1000), p = c(10, 100, 2000))
  
  apply(X = dimensions, MARGIN = 1, FUN = function(row) {
    n <- row[["n"]]
    p <- row[["p"]]
    
    plt <- plot_metric(plot_results, "train_mse", facet = c("type", "st_dev"),
                color = "corr", ylabel = "Mean Train MSE", fixy = FALSE,
                large_text = FALSE, n = n, p = p)
    save_plot(plot = plt,
              filename = paste("facet_train_mse_", response, "_", n, "_", p, sep = ""),
              path = paste(directory, "train-mse", sep = ""))
    
    return(plt)
  })
  
  apply(X = dimensions, MARGIN = 1, FUN = function(row) {
    n <- row[["n"]]
    p <- row[["p"]]
    
    plt <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                       color = "corr", ylabel = "Mean Test MSE", fixy = FALSE,
                       large_text = FALSE, n = n, p = p)
    save_plot(plot = plt,
              filename = paste("facet_test_mse_", response, "_", n, "_", p, sep = ""),
              path = paste(directory, "test-mse", sep = ""))
    
    return(plt)
  })
  
  apply(X = dimensions, MARGIN = 1, FUN = function(row) {
    n <- row[["n"]]
    p <- row[["p"]]
    
    plt <- plot_metric(accuracy_results, "sensitivity", facet = c("type", "st_dev"),
                       color = "corr", ylabel = expression(paste("Mean ", beta, "-sensitivity")),
                       fixy = TRUE, large_text = FALSE, n = n, p = p)
    save_plot(plot = plt,
              filename = paste("facet_sensitivity_", response, "_", n, "_", p, sep = ""),
              path = paste(directory, "sensitivity", sep = ""))
    
    return(plt)
  })
  
  apply(X = dimensions, MARGIN = 1, FUN = function(row) {
    n <- row[["n"]]
    p <- row[["p"]]
    
    plt <- plot_metric(accuracy_results, "specificity", facet = c("type", "st_dev"),
                       color = "corr", ylabel = expression(paste("Mean ", beta, "-specificity")),
                       fixy = TRUE, large_text = FALSE, n = n, p = p)
    save_plot(plot = plt,
              filename = paste("facet_specificity_", response, "_", n, "_", p, sep = ""),
              path = paste(directory, "specificity", sep = ""))
    
    return(plt)
  })
  
  # Create the publication plots
  
  publication_directory = "../../figures/publication-facet/"
  
  plt <- plot_metric(plot_results, "train_mse", facet = c("type", "st_dev"),
                     color = "corr", ylabel = "Mean Train MSE",
                     fixy = FALSE, large_text = TRUE, n = 50, p = 2000)
  save_plot(plot = plt,
            filename = paste("publication_facet_train-mse_", response, "_50_2000", sep = ""),
            path = publication_directory,
            width = 10, height = 10)
}


