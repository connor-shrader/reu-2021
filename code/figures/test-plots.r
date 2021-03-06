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

# Used to generate LaTeX tables of results.
library(tables) # v0.9.6


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
plot_metric <- function(data, metric, facet, color, ...) {
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  dat <- subset_data(data, ...)
  
  plt <- ggplot(data = dat) +
    geom_point(mapping = aes_string(x = "model_name", y = mean_metric, color = color, shape = color, fill = color), size = 2, alpha = 0.5) + 
    # geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    facet_grid(reformulate(facet[1], facet[2]), scales = "free_y", label = "label_parsed") +
    scale_shape_manual(values = 21:24, name = "Correlation") +
      scale_color_manual(values = hue_pal()(4), name = "Correlation") +
      scale_fill_manual(values = hue_pal()(4), name = "Correlation") +
      labs(x = "Model name", y = "Mean test MSE", color = "Correlation", shape = "Correlation") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.2),
      panel.grid = element_line(color = "gray90"),
      strip.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      strip.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.key = element_rect(fill = "white"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16)
    )
  
  return(plt)
}

aggregate_results <- readRDS("../../results/monte-carlo/aggregate_results.rds")
all_results <- readRDS("../../results/monte-carlo/all_results.rds")

# aggregate_results <- aggregate_results[aggregate_results$model_name != "gbm" &
#                                       aggregate_results$model_name != "rf" &
#                                       aggregate_results$model_name != "svm", ]

old_names <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
               "bsf", "ridge", "lasso", "enet", "adap_ridge",
               "adap_lasso", "adap_enet", "scad", "mcp", "gbm",
               "rf", "svm")
new_names <- c("OLS", "AIC back.", "BIC back.", "AIC step. back.",
               "BIC step. back.", "AIC for.", "BIC for.",
               "AIC step. for.", "BIC step. for.", "Ridge", "Lasso",
               "E-net", "Adap. ridge", "Adap. lasso", "Adap e-net",
               "SCAD", "MCP", "GB", "RF", "SVM")

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

Mean <- function(x) {
  if(length(x) == 0) {
    return(-1)
  }
  return(round(mean(x), 3))
}

SD <- function(x) {
  if(length(x) == 0) {
    return(-1)
  }
  return(round(sd(x), 3))
}

generate_table <- function(data, ...) {
  data <- subset_data(data, ...)
  # table_results contains the data used to generate a LaTeX summary table.
  # I defined this as a new variable so that aggregate_results is not overridden.
  table_results <- data[c("st_dev", "type", "corr", "model_name", "test_mse")]
  names(table_results) <- c("st.dev", "type", "corr", "model.name", "test.mse")
  
  # Create a tabular object from the tables library. The rows are layered by the
  # standard deviation and model name; the columns are layered by the type
  # of correlation and strength of correlation. The means and standard deviations
  # of the test MSE are shown.
  tab <- tabular((st.dev * model.name) ~ (type * corr * test.mse) * (Mean + SD), data = table_results)
  
  # The following two lines remove rows and columns that have a 0 as the first entry.
  # This removes unncessary rows and columns that aren't used for our plot/table.
  tab <- tab[tab[, 1] > 0, ]
  tab <- tab[, tab[1, ] > 0]
  
  # Call the following line to print out the LaTeX table. I could not get it to
  # save correctly to a file, so the output must be copy/pasted.
  print(toLatex(tab, options = list(justification = "l")))
}

plot_results <- aggregate_results

# Rename the st_dev column so that the plot has better facet labels.
plot_results$st_dev <- mapvalues(plot_results$st_dev,
                                      from = c("1", "3", "6"),
                                      to = c("sigma == 1", "sigma == 2", "sigma == 3"))
plot_results <- arrange(plot_results, corr)

# generate_table(all_results, n = 50, p = 10)
fig_for_n50_p10 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 50, p = 10)

# generate_table(all_results, n = 200, p = 10)
fig_for_n200_p10 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 200, p = 10)

# generate_table(all_results, n = 1000, p = 10)
fig_for_n1000_p10 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 1000, p = 10)

# generate_table(all_results, n = 50, p = 100)
fig_for_n50_p100 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 50, p = 100)

# generate_table(all_results, n = 200, p = 100)
fig_for_n200_p100 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 200, p = 100)

# generate_table(all_results, n = 1000, p = 100)
fig_for_n1000_p100 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 1000, p = 100)

# generate_table(all_results, n = 50, p = 2000)
fig_for_n50_p2000 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 50, p = 2000)

# generate_table(all_results, n = 200, p = 2000)
fig_for_n200_p2000 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 200, p = 2000)

# generate_table(all_results, n = 1000, p = 2000)
fig_for_n1000_p2000 <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                                 color = "corr", n = 1000, p = 2000)
  
ggsave(
 filename = "facet_50_10.png",
 path = "./images",
 plot = fig_for_n50_p10,
 type = "cairo-png",
 width = 10,
 height = 6,
 unit = "in"
)

ggsave(
  filename = "facet_50_10.eps",
  path = "./images",
  plot = fig_for_n50_p10,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_200_10.png",
  path = "./images",
  plot = fig_for_n200_p10,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_200_10.eps",
  path = "./images",
  plot = fig_for_n200_p10,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_1000_10.png",
  path = "./images",
  plot = fig_for_n1000_p10,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_1000_10.eps",
  path = "./images",
  plot = fig_for_n1000_p10,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_50_100.png",
  path = "./images",
  plot = fig_for_n50_p100,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_50_100.eps",
  path = "./images",
  plot = fig_for_n50_p100,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_200_100.png",
  path = "./images",
  plot = fig_for_n200_p100,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_200_100.eps",
  path = "./images",
  plot = fig_for_n200_p100,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_1000_100.png",
  path = "./images",
  plot = fig_for_n1000_p100,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_1000_100.eps",
  path = "./images",
  plot = fig_for_n1000_p100,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_50_2000.png",
  path = "./images",
  plot = fig_for_n50_p2000,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_50_2000.eps",
  path = "./images",
  plot = fig_for_n50_p2000,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_200_2000.png",
  path = "./images",
  plot = fig_for_n200_p2000,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_200_2000.eps",
  path = "./images",
  plot = fig_for_n200_p2000,
  width = 10,
  height = 6,
  unit = "in"
)



ggsave(
  filename = "facet_1000_2000.png",
  path = "./images",
  plot = fig_for_n1000_p2000,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "facet_1000_2000.eps",
  path = "./images",
  plot = fig_for_n1000_p2000,
  width = 10,
  height = 6,
  unit = "in"
)

