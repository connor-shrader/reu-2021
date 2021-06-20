# metrics.r
# Gabe Ackall, Seongtae Kim, Connor Shrader

# This file contains functions to evaluate the performance of linear regression.
# models. To generate data and fit the models, see the functions in
# simulation.r.

# FUNCTIONS
#   test_mse()
#   coefficient_bias()
#   compare_coefficient_estimates() (helper function)
#   confusion_matrix() (helper)
#   generate_confusion_matrices()
#   x_dif_2()
#   sample_mean()
#   sample_var()

# R version: 4.1.0
library(tidyverse) # v1.3.1
library(dplyr) # v1.0.6
library(faux) # v1.0.0
library(ncvreg) # v3.13.0
library(glmnet) # v4.1-1
library(MASS) # v7.3-54



# This function inputs a model and test data. It then computes and returns
# the mean squared error.
mean_squared_error <- function(model, test_dat) {
  if (class(model) == "cv.ncvreg") { #checks for mcp or scad model
    y_hat <-  data.frame(predict(model, X = as.matrix(test_dat[,-1])))
  }
  else if (class(model) == "cv.glmnet") { #check for lasso, ridge, enet model
    y_hat <-  data.frame(predict(model, newx = as.matrix(test_dat[,-1])))
  }
  else if (class(model)[1] == "H2ORegressionModel") { #check for H2O model
    print(h2o.mse(h2o.performance(model, newdata = as.h2o(test_dat))))
    return(h2o.mse(h2o.performance(model, newdata = as.h2o(test_dat))))
  }
  else { #rest are lm models
    y_hat <- data.frame(predict(model, newdata = test_dat[,-1]))
  }
  
  y <- test_dat[,1]
  mse <- mean(((y - y_hat)^2)[,1]) #take mean of residuals squared
  return(mse)
}



# This function inputs a result table (from calling results_table())
# and the name of a column and computes the Euclidean distance between
# results$soln and results$col. This gives an estimate for the bias for the
# coefficients of a model.
coefficient_bias <- function(results, col) {
  return(sqrt(sum((results$soln - results[[col]])^2)))
}



# This function is a helper method for the confusion_matrices() function. It assumes
# that the inputs are columns from a dataframe containing only zeros and ones (which
# represent whether a model predicted zero for a coefficient or non-zero). This function
# then creates a confusion matrix where the actual coefficient values are found
# from the "actual" column.
individual_confusion_matrix <- function(actual, prediction) {
  predicted_variables <- factor(prediction, levels = 0:1)
  true_variables <- factor(actual, levels = 0:1)
  confusionMatrix(data = predicted_variables, reference = true_variables, positive = "1")
}



# This function inputs a dataframe of coefficient estimates and outputs a list of
# confusion matrices. Each confusion matrix represents whether each model correctly
# guessed if each coefficient is non-zero or zero.
confusion_matrices <- function(coefs) {
  coef_is_nonzero <- as.data.frame(ifelse(coefs != 0, 1, 0))
  apply(X = coef_is_nonzero, FUN = individual_confusion_matrix, MARGIN = 2, actual = coef_is_nonzero$soln)
}



# Calculate sample variance of the coefficients
x_dif_2 <- function(coef_df, model){
  x_hat <- coef_df[["coefficients"]][model]
  betas <- coef_df[["coefficients"]]["soln"]
  x_difference <- betas - x_hat
  return(x_difference^2)
}



sample_mean <- function(row, df){  #takes sample mean: sum(x)/(n-1)
  total <- sum(df[row,])
  n <- length(df[row, ])
  samp_avg <- total / (n-1)
  return(samp_avg)
}



sample_var <- function(model, coefs_list){
  x_dif_df <-  lapply(coefs_list, x_dif_2, model = model)
  x_dif_df <- as.data.frame(x_dif_df)
  names_list <- rownames(x_dif_df)
  coef_variance <- lapply(names_list, sample_mean, df = x_dif_df) #returns list with sample variance for each coefficient in model
  return(coef_variance)
}