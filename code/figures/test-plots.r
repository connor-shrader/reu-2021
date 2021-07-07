# test-plots.r
# Gabe Ackall, Connor Shrader

# This file is used to experiment with plotting results

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

# Used to create plots
library(ggplot2) # v3.3.3

# plyr is used to map the values of the model_names column to more readable
# values.
library(plyr) # v1.8.6

# Used to melt and cast data frames into a format appropriate for LaTeX tables.
library(reshape) # v0.8.8

# Used to get the default color/fill scales for ggplot2.
library(scales) # v1.1.1

# This function takes in a data frame and a list of keyword arguments (...).
# This function then returns a subset of this dataframe that only contains rows
# where all the keyword arguments are satisfied. For example, if one calls
# subset_data(df, x = 3), then this function returns the rows of df such that
# df$x == 3.
subset_data <- function(data, ...) {
  args <- list(...)
  
  for (fixed_parameter in names(args)) {
    data <- data[data[[fixed_parameter]] == args[[fixed_parameter]], ]
  }
  
  return(data)
}

# This function takes in a data frame, a string representing a metric, and
# a column to use as a facet. This function then returns a facet grid of
# the data using ggplot2.
plot_metric <- function(data, metric, facet, ...) {
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
plot_metric_2 <- function(data, metric, facet, color, ...) {
  args <- list(...)
  
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  data <- subset_data(data, ...)
  
  plt <- ggplot(data = data) +
    geom_point(mapping = aes_string(x = "model_name", y = mean_metric, color = color, shape = color, fill = color), size = 2, alpha = 0.5) + 
    # geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    facet_grid(reformulate(facet[1], facet[2]), scales = "free_y", label = "label_parsed") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  
  return(plt)
}

plot_table <- function(data, id, measured, tab, ...) {
  data <- subset_data(data, ...)
  
  melted_table <- melt(sub_results, id = id, measured = measured)
  reshaped_table <- cast(data, tab)
}

aggregate_results <- readRDS("../../results/monte-carlo/aggregate_results.rds")

#aggregate_results <- aggregate_results[aggregate_results$model_name != "gbm" &
#                                       aggregate_results$model_name != "rf" &
#                                       aggregate_results$model_name != "svm", ]

#aggregate_results <- aggregate_results[aggregate_results$type != "independent", ]

#aggregate_results$corr <- factor(aggregate_results$corr)
#aggregate_results$type <- factor(aggregate_results$type, levels = c("independent", "symmetric", "autoregressive", "blockwise"))

sub_results <- subset_data(aggregate_results, p = 100, n = 200)[c("st_dev", "type", "corr", "model_name", "mean_test_mse")]
x <- melt(sub_results, id = c("type", "corr", "model_name", "st_dev"), measured = "test_mse")
y <- cast(x, st_dev + model_name ~ type + corr)

aggregate_results$model_name <- mapvalues(aggregate_results$model_name,
                  from = c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
                           "bsf", "ridge", "lasso", "enet", "adap_ridge",
                           "adap_lasso", "adap_enet", "scad", "mcp", "gbm",
                           "rf", "svm"),
                  to = c("OLS", "AIC back.", "BIC back.", "AIC step. back.",
                         "BIC step. back.", "AIC for.", "BIC for.",
                         "AIC step. for.", "BIC step. for.", "Ridge", "Lasso",
                         "E-net", "Adap. ridge", "Adap. lasso", "Adap e-net",
                         "SCAD", "MCP", "GB", "RF", "SVM"))

aggregate_results$st_dev <- mapvalues(aggregate_results$st_dev,
                                      from = c("1", "3", "6"),
                                      to = c("sigma == 1", "sigma == 2", "sigma == 3"))

aggregate_results <- arrange(aggregate_results, corr)

test_fig <- plot_metric_2(aggregate_results, "test_mse", facet = c("type", "st_dev"), color = "corr", p = 100, n = 200) +
  scale_shape_manual(values = 21:24, name = "Correlation") +
  scale_color_manual(values = hue_pal()(4), name = "Correlation") +
  scale_fill_manual(values = hue_pal()(4), name = "Correlation") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.2),
    panel.grid = element_line(color = "gray90"),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16)
  ) +
  labs(x = "Model name", y = "Mean test MSE", color = "Correlation", shape = "Correlation")





ggsave(
  filename = "facet.png",
  path = "./images",
  plot = test_fig,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)
