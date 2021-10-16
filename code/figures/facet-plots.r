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

library(reshape2)

library(ggh4x)

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

newer_plot_metric <- function(data, metric, small = FALSE, rescale = FALSE, ...) {
  
  
  dat <<- subset_data(data, ...)
  
  metric <- paste("mean_", metric, sep = "")
  
  if (metric == "mean_sensitivity" || metric == "mean_specificity") {
    
    dat <- dat[!dat$model_name %in% c("fm", "ridge", "gbm", "rf", "svm"), ]
  }
  
  # Replace model names with more readable names.
  old_names <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
                 "bsf", "ridge", "lasso", "enet", "scad", "mcp", "gbm",
                 "rf", "svm")
  new_names <- c("OLS", "AIC B", "BIC B", "AIC SB",
                 "BIC SB", "AIC F", "BIC F",
                 "AIC SF", "BIC SF", "Ridge", "Lasso",
                 "E-net", "SCAD", "MCP", "XGBoost", "RF", "SVM")
  
  dat$model_name <- mapvalues(dat$model_name,
                              from = old_names, to = new_names)
  
  # Rename the st_dev column so that the plot has better facet labels.
  old_st_dev <- c("1", "3", "6")
  new_st_dev <- c("sigma == 1", "sigma == 3", "sigma == 6")
  dat$st_dev <- mapvalues(dat$st_dev, from = old_st_dev, to = new_st_dev)
  
  dat <- arrange(dat, corr)
  
  dat$type <- mapvalues(dat$type,
                        from = c("independent", "symmetric",
                                 "autoregressive", "blockwise"),
                        to = c("Independent", "Symmetric",
                               "Autoregressive", "Blockwise"))
  
  need_to_rescale <- rescale == TRUE && 
    (metric == "mean_train_mse" || metric == "mean_test_mse")
  if (need_to_rescale) {
    max1 <<- max(max(dat[dat$st_dev == "sigma == 1",]$mean_train_mse),
                 max(dat[dat$st_dev == "sigma == 1",]$mean_test_mse))
    min1 <<- 0
    max3 <<- max(max(dat[dat$st_dev == "sigma == 3",]$mean_train_mse),
                 max(dat[dat$st_dev == "sigma == 3",]$mean_test_mse))
    min3 <<- 0
    max6 <<- max(max(dat[dat$st_dev == "sigma == 6",]$mean_train_mse),
                 max(dat[dat$st_dev == "sigma == 6",]$mean_test_mse))
    min6 <<- 0
    
    df2 <- data.frame(model_name = rep("Lasso", 6), 
                      type = rep("Independent", 6),
                      st_dev = c("1", "1", "3", "3", "6", "6"))
    df2[[metric]] <- c(max1, min1, max3, min3, max6, min6)
    df2$st_dev <- mapvalues(df2$st_dev, from = old_st_dev, to = new_st_dev)
    
    #df2$type <- factor(df2$type, levels = c("Independent", "Symmetric",
    #                                        "Autoregressive", "Blockwise"))
  }
  
  ylabel <- NULL
  if (metric == "mean_train_mse") {
    ylabel <- "Mean Training MSE"
  }
  else if (metric == "mean_test_mse") {
    ylabel <- "Mean Test MSE"
  }
  else if (metric == "mean_sensitivity") {
    ylabel <- expression(paste("Mean ", beta, "-sensitivity"))
  }
  else if (metric == "mean_specificity") {
    ylabel <- expression(paste("Mean ", beta, "-specificity"))
  }
  
  plt <- ggplot(data = dat)
  
  if (small) {
    plt <- plt +
      geom_point(
        mapping = aes_string(
          x = "model_name",
          y = metric,
          color = "corr",
          shape = "corr",
          fill = "corr"),
        size = 4)
  }
  else {
    plt <- plt +
      geom_point(
        mapping = aes_string(
          x = "model_name",
          y = metric,
          color = "corr",
          shape = "corr",
          fill = "corr"),
        size = 2)
  }
  
  if(need_to_rescale) {
    plt <- plt + 
      geom_point(
        mapping = aes_string(
          x = "model_name",
          y = metric),
        data = df2, alpha = 0)
  }
  
  plt <- plt +
    scale_shape_manual(values = 21:24, name = "Correlation") +
    scale_color_manual(values = hue_pal()(4), name = "Correlation") +
    scale_fill_manual(values = hue_pal()(4), name = "Correlation") +
    labs(x = "Model", y = ylabel, color = "Correlation", shape = "Correlation") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.2),
      panel.grid = element_line(color = "gray90"),
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    )
  
  if(small == TRUE) {
    plt <- plt + theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 30),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.position="bottom",
      legend.box = "horizontal"
    )
  }
  
  if (metric == "mean_train_mse" || metric == "mean_test_mse") {
    plt <- plt + facet_grid(st_dev ~ type, scales = "free_y", label = "label_parsed")
  }
  else {
    plt <- plt + facet_grid(st_dev ~ type, label = "label_parsed")
  }
  
  return(plt)
}

new_plot_metric <- function(data, metric, ...) {
  
  dat <<- subset_data(data, ...)
  
  # Given plot_results
  
  col_names <- names(plot_results)
  
  newdat <<- 2
  if (metric == "mse") {
    col_names_kept <- col_names[col_names != "mean_train_mse" & col_names != "mean_test_mse"]
    
    newdat <<- melt(dat, id.vars = col_names_kept, value.name = "mean_mse")
    
    newdat$variable <<- mapvalues(newdat$variable, from = c("mean_train_mse", "mean_test_mse"),
              to = c("Training", "Test"))
    
    ylabel <- "Mean MSE"
  }
  else if (metric == "beta") {
    col_names_kept <- col_names[col_names != "mean_sensitivity" & col_names != "mean_specificity"]
    
    newdat <<- melt(dat, id.vars = col_names_kept, value.name = "mean_beta")
  }
  else {
    stop("Metric should be 'mse' or 'beta'.")
  }
  
  plt <- ggplot(data = newdat) +
    geom_point(mapping = aes(
      x = model_name,
      y = mean_mse,
      color = corr,
      shape = corr,
      fill = corr
    )) +
    scale_shape_manual(values = 21:24, name = "Correlation") +
    scale_color_manual(values = hue_pal()(4), name = "Correlation") +
    scale_fill_manual(values = hue_pal()(4), name = "Correlation") +
    labs(x = "Model", y = ylabel, color = "Correlation", shape = "Correlation") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.2),
      panel.grid = element_line(color = "gray90"),
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 30),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.position="bottom",
      legend.box = "horizontal"
    )
  
  if (metric == "mean_train_mse" || metric == "mean_test_mse") {
    plt <- plt + facet_grid(reformulate(facet[1], facet[2]), scales = "free_y", label = "label_parsed")
  }
  else {
    plt <- plt + facet_grid(reformulate(facet[1], facet[2]), label = "label_parsed")
  }
  
  return(plt)
}

# This function is a more complete version of plot_metric.
plot_metric <- function(data, metric, facet, color, ylabel = "Mean test MSE", 
                        fixy = FALSE, large_text = FALSE, ...) {
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  dat <- subset_data(data, ...)
  
  
  
  plt <- ggplot(data = dat) 
  
  if (large_text == TRUE) {
    plt <- plt + geom_point(
      mapping = aes_string(x = "model_name",
                           y = mean_metric, 
                           color = color, 
                           shape = color, 
                           fill = color), 
      size = 4)
  }
  else {
    plt <- plt + geom_point(
      mapping = aes_string(x = "model_name",
                           y = mean_metric, 
                           color = color, 
                           shape = color, 
                           fill = color), 
      size = 2)
  }
  
  plt <- plt +
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
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    )
  
  if(large_text == TRUE) {
    plt <- plt + theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20),
      axis.text.y = element_text(size = 20),
      strip.text = element_text(size = 20),
      axis.title = element_text(size = 30),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),
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

n <- c(50, 200, 1000)
p <- c(10, 100, 2000)
response <- c("linear", "nonlinear")
metrics <- c("train_mse", "test_mse", "sensitivity", "specificity")

dimensions <- expand.grid(n = n, p = p, response = response)

for (row in 1:nrow(dimensions)) {
  n <- dimensions$n[row]
  p <- dimensions$p[row]
  response <- dimensions$response[row]
  file <- paste("../../results/monte-carlo-", response, "/aggregate_",
                response, "_results.rds", sep = "")
  directory <- paste("../../figures/", response, "-facet/", sep = "")

  results <- readRDS(file)

  for (metric in metrics) {
    plt <- newer_plot_metric(results, metric, small = FALSE, rescale = FALSE,
                             n = n, p = p)

    filename <- paste("facet", metric, sep = "_")
    folder <- sub("_", "-", metric)
    save_plot(plot = plt,
              filename = paste(filename, response, n, p, sep = "_"),
              path = paste(directory, folder, sep = ""))
  }
}


response <- c("linear", "nonlinear")
metrics <- c("train_mse", "test_mse", "sensitivity", "specificity")

dimensions <- expand.grid(metric = metrics, response = response)
publication_directory = "../../figures/publication-facet/"

for (row in 1:nrow(dimensions)) {
  metric <- dimensions$metric[row]
  response <- dimensions$response[row]
  
  file <- paste("../../results/monte-carlo-", response, "/aggregate_",
                response, "_results.rds", sep = "")
  
  results <- readRDS(file)
  
  plt <- newer_plot_metric(results, metric, small = TRUE, rescale = TRUE,
                     n = 50, p = 2000)
  
  filename <- paste("publication_facet", metric, response, "50_2000", sep = "_")
  save_plot(plot = plt,
            filename = filename,
            path = publication_directory,
            width = 10, height = 10)
}

stop("STOP")

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
  
  # aggregate_results <- aggregate_results[aggregate_results$model_name != "gbm" &
  #                                       aggregate_results$model_name != "rf" &
  #                                       aggregate_results$model_name != "svm", ]
  
  # old_names <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
  #                "bsf", "ridge", "lasso", "enet", "scad", "mcp", "gbm",
  #                "rf", "svm")
  # new_names <- c("OLS", "AIC B", "BIC B", "AIC SB",
  #                "BIC SB", "AIC F", "BIC F",
  #                "AIC SF", "BIC SF", "Ridge", "Lasso",
  #                "E-net", "SCAD", "MCP", "XGBoost", "RF", "SVM")
  # 
  # # Replace model names with more readable names.
  # aggregate_results$model_name <- mapvalues(aggregate_results$model_name,
  #                                           from = old_names, to = new_names)
  # 
  # all_results$model_name <- mapvalues(all_results$model_name,
  #                                     from = old_names, to = new_names)
  # 
  # # Below is some old code used to generate a table to put into the final report.
  # # I ended up deciding to use the tabular() function from the tables library
  # # instead.
  # 
  # # sub_results <- subset_data(aggregate_results, p = 100, n = 200)[c("st_dev", "type", "corr", "model_name", "mean_test_mse", "sd_test_mse")]
  # # x <- melt(sub_results, id = c("type", "corr", "model_name", "st_dev"), measured = c("mean_test_mse", "sd_test_mse"))
  # # y <- cast(x, st_dev + model_name ~ type + corr)
  # 
  # plot_results <- aggregate_results
  # 
  # # Rename the st_dev column so that the plot has better facet labels.
  # plot_results$st_dev <- mapvalues(plot_results$st_dev,
  #                                  from = c("1", "3", "6"),
  #                                  to = c("sigma == 1", "sigma == 3", "sigma == 6"))
  
  # Capitalize the type column
  # plot_results$type <- mapvalues(plot_results$type,
  #                                from = c("independent", "symmetric",
  #                                         "autoregressive", "blockwise"),
  #                                to = c("Independent", "Symmetric",
  #                                       "Autoregressive", "Blockwise"))
  
  # plot_results <- arrange(plot_results, corr)
  accuracy_results <- aggregate_results[!aggregate_results$model_name %in% c("fm",
                                   "ridge", "gbm", "rf", "svm"), ]
  
  dimensions <- expand.grid(n = c(50, 200, 1000), p = c(10, 100, 2000))
  
  
  # Train MSE
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
  
  # Test MSE
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
  
  # Sensitivity
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
  
  # Specificity
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
  
  # Train MSE
  plt <- plot_metric(plot_results, "train_mse", facet = c("type", "st_dev"),
                     color = "corr", ylabel = "Mean Train MSE",
                     fixy = FALSE, large_text = TRUE, n = 50, p = 2000)
  save_plot(plot = plt,
            filename = paste("publication_facet_train_mse_", response, "_50_2000", sep = ""),
            path = publication_directory,
            width = 10, height = 10)
  
  # Test MSE
  plt <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                     color = "corr", ylabel = "Mean Test MSE",
                     fixy = FALSE, large_text = TRUE, n = 50, p = 2000)
  save_plot(plot = plt,
            filename = paste("publication_facet_test_mse_", response, "_50_2000", sep = ""),
            path = publication_directory,
            width = 10, height = 10)
  
  # Sensitivity
  plt <- plot_metric(accuracy_results, "sensitivity", facet = c("type", "st_dev"),
                     color = "corr", ylabel = expression(paste("Mean ", beta, "-sensitivity")),
                     fixy = TRUE, large_text = TRUE, n = 50, p = 2000)
  save_plot(plot = plt,
            filename = paste("publication_facet_sensitivity_", response, "_50_2000", sep = ""),
            path = publication_directory,
            width = 10, height = 10)
  
  # Specificity
  plt <- plot_metric(accuracy_results, "specificity", facet = c("type", "st_dev"),
                     color = "corr", ylabel = expression(paste("Mean ", beta, "-specificity")),
                     fixy = TRUE, large_text = TRUE, n = 50, p = 2000)
  save_plot(plot = plt,
            filename = paste("publication_facet_specificity_", response, "_50_2000", sep = ""),
            path = publication_directory,
            width = 10, height = 10)
}


