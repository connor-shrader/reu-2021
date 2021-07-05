# test-plots.r

# This file is used to experiment with plotting results

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

library(ggplot2)

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
    facet_grid(reformulate(".", facet)) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
}


aggregate_results <- readRDS("../../results/monte-carlo/aggregate_results.rds")

plot_metric(aggregate_results, "test_mse", facet = "st_dev", p = 10, type = "independent", n = 50)
