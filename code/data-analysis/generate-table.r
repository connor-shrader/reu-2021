# test-plots.r
# Gabe Ackall, Connor Shrader

# This file is used to generate result tables to be included in tex files.

rm(list = ls())
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("./subset-data.r")

# Used to melt and cast data frames into a format appropriate for LaTeX tables.
library(reshape) # v0.8.8

# Used to generate LaTeX tables of results.
library(tables) # v0.9.6

# plyr is used to map the values of the model_names column to more readable
# values.
library(plyr) # v1.8.6

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

generate_raw_table <- function(dat, metric, ...) {
  print(metric)
  dat <<- subset_data(dat, ...)
  dat <<- dat[!is.na(dat[[metric]]), ]
  
  # table_results contains the data used to generate a LaTeX summary table.
  table_results <<- dat[c("st_dev", "type", "corr", "model_name", metric)]
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
  tab <<- tab
  tab <- tab[tab[, 1] > -1, ]
  tab <- tab[, tab[1, ] > -1]
  
  # Call the following line to print out the LaTeX table. I could not get it to
  # save correctly to a file, so the output must be copy/pasted.
  return(toLatex(tab, options = list(justification = "l"))$text)
  return(tab)
}

repair_table <- function(table_string) {
  old_header_index <- regexpr(pattern = "\n1", table_string)
  file_name <- "./table-header.txt"
  header <- readChar(file_name, nchars = 1000)
  new_string <- paste(header, substring(table_string, old_header_index + 1))
  
  new_string <- sub(pattern = "\\\\\n3", replacement = "\\\\\\\\hline\n3", new_string)
  new_string <- sub(pattern = "\\\\\n6", replacement = "\\\\\\\\hline\n6", new_string)
  return(new_string)
}

generate_table <- function(dat, metric, filename, path, ...) {
  raw_table <- generate_raw_table(dat, metric, ...)
  repaired_table <- repair_table(raw_table)
  
  last_char <- substr(path, nchar(path), nchar(path))
  if (last_char != "/") {
    path <- paste(path, "/", sep = "")
  }
  
  directory <- paste(path, filename, ".tex", sep = "")
  print(directory)
  write(repaired_table, file = directory)
  print("Saved a table")
}

all_linear_results <- readRDS("../../results/monte-carlo-linear/all_linear_results.rds")
all_nonlinear_results <- readRDS("../../results/monte-carlo-nonlinear/all_nonlinear_results.rds")



old_names <- c("fm", "ab", "bb", "asb", "bsb", "af", "bf", "asf",
               "bsf", "ridge", "lasso", "enet", "scad", "mcp", "gbm",
               "rf", "svm")
new_names <- c("OLS", "AIC B", "BIC B", "AIC SB",
               "BIC SB", "AIC F", "BIC F",
               "AIC SF", "BIC SF", "Ridge", "Lasso",
               "E-net", "SCAD", "MCP", "XGBoost", "RF", "SVM")

# Replace model names with more readable names.
all_linear_results$model_name <- mapvalues(all_linear_results$model_name,
                                           from = old_names, to = new_names)

all_nonlinear_results$model_name <- mapvalues(all_nonlinear_results$model_name,
                                              from = old_names, to = new_names)

old_type <- c("independent", "symmetric",
              "autoregressive", "blockwise")

new_type <- c("Independent", "Symmetric",
              "Autoregressive", "Blockwise")

all_linear_results$type <- mapvalues(all_linear_results$type,
                               from = old_type, to = new_type)

all_nonlinear_results$type <- mapvalues(all_linear_results$type,
                                     from = old_type, to = new_type)

# Run a line like the following to generate a table:
# table_string <- generate_table(all_linear_results, metric = "train_mse", n = 1000, p = 10)

# table_string <- generate_table(all_linear_results, metric = "train_mse", 
#                                filename = "test", path = "./",
#                                n = 1000, p = 10)

dimensions <- expand.grid(n = c(50, 200, 1000),
                          p = c(10, 100, 2000),
                          type = c("train_mse", "test_mse", "sensitivity", "specificity"),
                          response = c(1, 2))

dimensions <- dimensions[19:72, ]

tables <- apply(X = dimensions, MARGIN = 1, FUN = function(row) {
  n <- strtoi(row[["n"]])
  p <- strtoi(row[["p"]])
  type <- row[["type"]]
  response <- row[["response"]]
  
  if (type == "train_mse") {
    folder = "train-mse"
  }
  else if (type == "test_mse") {
    folder = "test-mse"
  } 
  else {
    folder = type
  }
  
  filename <- paste("facet", type, response, n, p, sep = "_")
  path <- paste("../../tables", ifelse(response == 1, "linear-facet", "nonlinear-facet"),
                folder, sep = "/")
  
  print(filename)
  print(path)
  
  if (response == 1) {
    generate_table(all_linear_results, metric = type, 
                   filename = filename, path = path,
                   n = n, p = p)
  }
  else {
    generate_table(all_nonlinear_results, metric = type, 
                   filename = filename, path = path,
                   n = n, p = p)
  }
})