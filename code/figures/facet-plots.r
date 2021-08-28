# test-plots.r
# Gabe Ackall, Connor Shrader

# This file is used to experiment with plotting results

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

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

library(caret)


# This function takes in a data frame and a list of keyword arguments (...).
# This function then returns a subset of this dataframe that only contains rows
# where all the keyword arguments are satisfied. For example, if one calls
# subset_data(df, x = 3), then this function returns the rows of df such that
# df$x == 3.
subset_data <- function(data, ...) {
  args <- list(...)
  
  dat <- data
  for (fixed_parameter in names(args)) {
    dat <- dat[dat[[fixed_parameter]] == args[[fixed_parameter]], ]
  }
  
  return(dat)
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
plot_metric <- function(data, metric, facet, color, ylabel = "Mean test MSE", fixy = FALSE, ...) {
  mean_metric <- paste("mean_", metric, sep = "")
  sd_metric <- paste("sd_", metric, sep = "")
  
  dat <- subset_data(data, ...)
  
  plt <- ggplot(data = dat) +
    geom_point(mapping = aes_string(x = "model_name", y = mean_metric, color = color, shape = color, fill = color), size = 2) + 
    # geom_errorbar(mapping = aes_string(x = "model_name", y = mean_metric, ymin = paste(mean_metric, "-", sd_metric), ymax = paste(mean_metric, "+", sd_metric))) +
    scale_shape_manual(values = 21:24, name = "Correlation") +
    scale_color_manual(values = hue_pal()(4), name = "Correlation") +
    scale_fill_manual(values = hue_pal()(4), name = "Correlation") +
    labs(x = "Model name", y = ylabel, color = "Correlation", shape = "Correlation") +
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

Mean2 <- function(x) {
  if(length(x) == 0) {
    return(-1)
  }
  return(round(mean(x), 2))
}

SD2 <- function(x) {
  if(length(x) == 0) {
    return(-1)
  }
  return(round(sd(x), 2))
}

Mean4 <- function(x) {
  if(length(x) == 0) {
    return(-1)
  }
  return(round(mean(x), 4))
}

SD4 <- function(x) {
  if(length(x) == 0) {
    return(-1)
  }
  return(round(sd(x), 4))
}

# generate_table(all_results, metric = "train_mse", n = 1000, p = 10)
generate_table <- function(data, metric, ...) {
  data <- subset_data(data, ...)
  data <- data[!is.na(data[[metric]]), ]
  
  # table_results contains the data used to generate a LaTeX summary table.
  table_results <- data[c("st_dev", "type", "corr", "model_name", metric)]
  table_results[[metric]] <- as.numeric(table_results[[metric]])
  
  # Create a tabular object from the tables library. The rows are layered by the
  # standard deviation and model name; the columns are layered by the type
  # of correlation and strength of correlation. The means and standard deviations
  # of the metric are computed.
  if (metric == "train_mse") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "train.mse")
    tab <- tabular((st.dev * model.name) ~ (type * corr * train.mse) * (Mean2 + SD2), data = table_results)
  }
  else if (metric == "test_mse") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "test.mse")
    tab <- tabular((st.dev * model.name) ~ (type * corr * test.mse) * (Mean2 + SD2), data = table_results)
  }
  else if (metric == "tn") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "tn")
    tab <- tabular((st.dev * model.name) ~ (type * corr * tn) * (Mean2 + SD2), data = table_results)
  }
  else if (metric == "fn") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "fn")
    tab <- tabular((st.dev * model.name) ~ (type * corr * fn) * (Mean2 + SD2), data = table_results)
  }
  else if (metric == "fp") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "fp")
    tab <- tabular((st.dev * model.name) ~ (type * corr * fp) * (Mean2 + SD2), data = table_results)
  }
  else if (metric == "tp") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "tp")
    tab <- tabular((st.dev * model.name) ~ (type * corr * tp) * (Mean2 + SD2), data = table_results)
  }
  else if (metric == "sensitivity") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "sensitivity")
    tab <- tabular((st.dev * model.name) ~ (type * corr * sensitivity) * (Mean4 + SD4), data = table_results)
  }
  else if (metric == "specificity") {
    names(table_results) <- c("st.dev", "type", "corr", "model.name", "specificity")
    tab <- tabular((st.dev * model.name) ~ (type * corr * specificity) * (Mean4 + SD4), data = table_results)
  }
  else {
    stop(paste("Did not recognize the metric ", metric, "."))
  }
  
  # The following two lines remove rows and columns that have a 0 as the first entry.
  # This removes unncessary rows and columns that aren't used for our plot/table.
  tab <- tab[tab[, 1] > -1, ]
  tab <- tab[, tab[1, ] > -1]
  
  # Call the following line to print out the LaTeX table. I could not get it to
  # save correctly to a file, so the output must be copy/pasted.
  print(toLatex(tab, options = list(justification = "l")))
  return(tab)
}

for (response in 2) {
  if (response == 1) {
    aggregate_results <- readRDS("../../results/monte-carlo/aggregate_results.rds")
    all_results <- readRDS("../../results/monte-carlo/all_results.rds")
    directory <- "./images/linear-facet/"
  }
  if (response == 2) {
    aggregate_results <- readRDS("../../results/monte-carlo-nonlinear/aggregate_nonlinear_results.rds")
    all_results <- readRDS("../../results/monte-carlo-nonlinear/all_nonlinear_results.rds")
    directory <- "./images/nonlinear-facet/"
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
  
  plot_results <- arrange(plot_results, corr)
  accuracy_results <- plot_results[!plot_results$model_name %in% c("OLS",
                                   "Ridge", "Adap. ridge", "XGBoost", "RF", "SVM"), ]
  
  dimensions <- expand.grid(n = c(50), p = c(10))
  
  apply(X = dimensions, MARGIN = 1, FUN = function(row) {
    n <- row[["n"]]
    p <- row[["p"]]
    
    plt <- plot_metric(plot_results, "train_mse", facet = c("type", "st_dev"),
                color = "corr", ylabel = "Mean Train MSE", fixy = FALSE, n = n, p = p)
    save_plot(plot = plt,
              filename = paste("facet_train_mse_", response, "_", n, "_", p, sep = ""),
              path = paste(directory, "train-mse", sep = ""))
    
    return(plt)
  })
  
  apply(X = dimensions, MARGIN = 1, FUN = function(row) {
    n <- row[["n"]]
    p <- row[["p"]]
    
    plt <- plot_metric(plot_results, "test_mse", facet = c("type", "st_dev"),
                       color = "corr", ylabel = "Mean Test MSE", fixy = FALSE, n = n, p = p)
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
                       fixy = TRUE, n = n, p = p)
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
                       fixy = TRUE, n = n, p = p)
    save_plot(plot = plt,
              filename = paste("facet_specificity_", response, "_", n, "_", p, sep = ""),
              path = paste(directory, "specificity", sep = ""))
    
    return(plt)
  })
}



