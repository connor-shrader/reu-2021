# test-plots.r

# This file is used to experiment with plotting results

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

library(ggplot2)

subset_data <- function(data, ...) {
  args <- list(...)
  
  for (fixed_parameter in names(args)) {
    data <- data[data[[fixed_parameter]] == args[[fixed_parameter]], ]
  }
  
  return(data)
}

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

plot_metric_2 <- function(data, metric, facet, color, ...) {
  args <- list(...)
  
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  data <- subset_data(data, ...)
  
  plt <- ggplot(data = data) +
    geom_point(mapping = aes_string(x = "model_name", y = mean_metric, color = color), size = 2, alpha = 0.5) + 
    # geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    facet_grid(reformulate(facet[1], facet[2]), scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  
  return(plt)
}


aggregate_results <- readRDS("../../results/monte-carlo/aggregate_results.rds")

#aggregate_results <- aggregate_results[aggregate_results$model_name != "gbm" &
#                                       aggregate_results$model_name != "rf" &
#                                       aggregate_results$model_name != "svm", ]

#aggregate_results <- aggregate_results[aggregate_results$type != "independent", ]

aggregate_results$corr <- factor(aggregate_results$corr)
aggregate_results$type <- factor(aggregate_results$type, levels = c("independent", "symmetric", "autoregressive", "blockwise"))

library(plyr)

aggregate_results$model_name <- mapvalues(aggregate_results$model_name,
                  from = c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
                           "bsf", "ridge", "lasso", "enet", "adap_ridge",
                           "adap_lasso", "adap_enet", "scad", "mcp", "gbm",
                           "rf", "svm"),
                  to = c("OLS", "AIC back.", "BIC back.", "AIC step. back.",
                         "BIC step. back.", "AIC for.", "BIC for.",
                         "AIC step. for.", "BIC step. for.", "Ridge", "Lasso",
                         "E-net", "Adap. ridge", "Adap. lasso", "Adap e-net",
                         "SCAD", "MCP", "Grad. Boost", "RF", "SVM"))

test_fig <- plot_metric_2(aggregate_results, "test_mse", facet = c("type", "st_dev"), color = "corr", p = 100, n = 200) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.2),
    panel.grid = element_line(color = "gray90"),
    strip.background = element_blank(),
    strip.text = element_text()
  ) +
  labs(x = "Model name", y = "Mean test MSE")

sub_results <- subset_data(aggregate_results, p = 100, n = 200)[c("st_dev", "type", "corr", "model_name", "mean_test_mse")]

# all_results <- readRDS("../../results/monte-carlo/all_results.rds")
# t <- all_results %>% group_by(st_dev, corr, type, model_name) %>% summarize(mean_test_mse = mean(test_mse))
x <- melt(sub_results, id = c("type", "corr", "model_name", "st_dev"), measured = "test_mse")
y <- cast(x, st_dev + model_name ~ type + corr)



# ggsave(
#   filename = "facet.png",
#   path = "./images",
#   plot = test_fig,
#   type = "cairo-png",
#   width = 10,
#   height = 6,
#   unit = "in"
# )
