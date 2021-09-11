# test-plots.r
# Gabe Ackall, Connor Shrader

# This file is used to experiment with plotting results

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("./subset-data.r")

# Used to melt and cast data frames into a format appropriate for LaTeX tables.
library(reshape) # v0.8.8

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

all_linear_results <- readRDS("../../results/monte-carlo-linear/all_linear_results.rds")
all_nonlinear_results <- readRDS("../../results/monte-carlo-nonlinear/all_nonlinear_results.rds")

# Run a line like the following to generate a table:
# generate_table(all_results, metric = "train_mse", n = 1000, p = 10)