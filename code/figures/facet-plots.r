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
    
    phantom <- data.frame(model_name = rep("Lasso", 6), 
                      type = rep("Independent", 6),
                      st_dev = c("1", "1", "3", "3", "6", "6"))
    phantom[[metric]] <- c(max1, min1, max3, min3, max6, min6)
    phantom$st_dev <- mapvalues(phantom$st_dev, from = old_st_dev, to = new_st_dev)
    
    phantom$type <- factor(phantom$type, levels = c("Independent", "Symmetric",
                                            "Autoregressive", "Blockwise"))
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
        data = phantom, alpha = 0)
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

#
#
# CREATING THE PLOTS
#
#

# Creating the supplementary figures

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
              path = paste(directory, folder, sep = ""), cairo = FALSE)
  }
}

# Creating the publication figures

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
            width = 10, height = 10,
            cairo = FALSE)
}