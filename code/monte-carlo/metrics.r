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
test_mse <- function(model, test_dat) {
  if (class(model) == "cv.ncvreg") { #checks for mcp or scad model
    y_hat <-  data.frame(predict(model, X = as.matrix(test_dat[,-1])))
  }
  else if (class(model) == "cv.glmnet") { #check for lasso, ridge, enet model
    y_hat <-  data.frame(predict(model, newx = as.matrix(test_dat[,-1])))
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
  return(sqrt(sum((results$soln - results$col)^2)))
}



# This helper function compares two boolean values and determines whether there
# is a true positive (tp), false negative (fn), false positive (fp), or
# true negative (tn). The first boolean is the actual value, while the second
# boolean is the predicted value. This function is used for
# generate_confusion_matrices().
compare_coefficient_estimate <- function(solution, prediction) {
  if (solution & prediction) {
    return("tp")
  }
  else if (solution & !prediction) {
    return("fn")
  }
  else if (!solution & prediction) {
    return("fp")
  }
  else {
    return("tn")
  }
}



# This function takes in a list of test results (true negative, false negative,
# false positive or true negative) and combines the sum of each result into
# a confusion matrix. Despite being called a confusion matrix, the returned
# object is a data frame. This function is used in generate_confusion_matrices().
confusion_matrix <- function(lis) {
  confusion_matrix <- data.frame(
    actual_negative = c(sum(lis == "tn"), sum(lis == "fp")),
    actual_positive = c(sum(lis == "fn"), sum(lis == "tp")),
    row.names = c("predicted_negative", "predicted_positive")
  )
  
  return(confusion_matrix)
}



# This function takes a table of coefficient estimates (from results_table())
# and computes the confusion matrix for each model. This function then returns a list
# containing these matrices.
generate_confusion_matrices <- function(coefs) {
  # Create a data frame where each entry is TRUE if the corresponding entry in
  # coefs is non-zero; otherwise, the entry is FALSE.
  coef_is_nonzero <- as.data.frame(ifelse(coefs == 0, FALSE, TRUE))
  
  # For each column of coef_is_nonzero, we compare its entries to the entries
  # in the "soln" column. This gives us a list whose entries have four possible values:
  # tn (true negative), false negative (fn), false positive (fp), or true
  # positive (tp). For example, if a certain model correctly predicts that a
  # coefficient will be non-zero, the corresponding entry will be set to "tp".
  test_results <- as.data.frame(
    apply(X = coef_is_nonzero,
          MARGIN = 2,
          FUN = function(prediction, solution) {
            mapply(compare_coefficient_estimate, solution, prediction)
          },
          solution = coef_is_nonzero$soln)
  )
  
  # For each column of test_results, compute the confusion matrix based on the
  # counts of tn, fn, fp, and tp. This then gets returned.
  matrices <- apply(X = test_results, MARGIN = 2, FUN = confusion_matrix)
  
  return(matrices)
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