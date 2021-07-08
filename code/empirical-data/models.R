# Analysis of bcTCGA breast cancer gene expression data

library(glmnet)
library(gcdnet)
library(ncvreg)
library(ranger)
library(xgboost)
library(e1071)
library(caret) #for cross validation data splitting
library(tidyverse) #data cleaning

#set seed
set.seed(12345)

# load data and set it up into a dataframe
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
cancer <- readRDS("bcTCGA.rds")
cancer[["X"]] <- scale(cancer[["X"]], center = TRUE, scale = TRUE)
cancer_df <- as.data.frame(cbind(cancer[["y"]], cancer[["X"]])) #turn matrix into dataframe

colnames(cancer_df)[1] <- "y" #rename y column to "y"

k_folds <- 5
cv_folds <- createFolds(cancer_df[,1], k = k_folds)

calc_mse <- function(model, dat) {
  # Obtain the first class for each model.
  model_class <- class(model)[1]
  
  # This if chain computes the predicted response variables. The syntax to
  # make predictions depends on the model used, since they come from different
  # libraries.
  if (model_class == "cv.ncvreg") { #checks for mcp or scad model
    # Model is SCAD or MCP.
    y_hat <-  data.frame(predict(model, X = as.matrix(dat[, -1])))
  }
  else if (model_class == "cv.glmnet" || model_class == "cv.gcdnet") { 
    # Model is LASSO, ridge, enet, or adaptive lasso/ridge/enet.
    y_hat <-  data.frame(predict(model, newx = as.matrix(dat[, -1])))
  }
  else if (model_class == "xgb.Booster") { 
    # Model is XGBoost.
    y_hat <- data.frame(predict(model, newdata = as.matrix(dat[, -1])))
  }
  else if (model_class == "ranger") { 
    # Model is random forest.
    predict_data <- predict(model, data = as.matrix(dat[, -1]))
    y_hat <- as.data.frame(predict_data$predictions)
  }
  else if (model_class == "svm") {
    # Model is SVM.
    y_hat <- data.frame(predict(model, newdata = as.matrix(dat[, -1])))
  }
  else { 
    # lm model.
    y_hat <- data.frame(predict(model, dat[,-1]))
  }
  
  # Get the actual response values.
  y <- dat[, 1]
  
  # Compute MSE.
  mse <- mean(((y - y_hat)^2)[, 1]) #take mean of residuals squared
  return(mse)
}


models <-  list()
runtimes <- list()
cv.mse <- list()
# perform cross validation of data
for (fold in seq(cv_folds)) {
  train_dat <- cancer_df[-cv_folds[[fold]],]
  test_dat <- cancer_df[cv_folds[[fold]],]
  
  models[[fold]] <-  list()
  runtimes[[fold]] <- list()
  cv.mse[[fold]] <- list()
  
  # run glmnet penalized regression techniques (lasso, ridge, and elastic net)
  # Lasso
  lasso_time <- system.time(lasso <- cv.glmnet(x = as.matrix(train_dat[,-1]), y = as.matrix(train_dat[,1]), alpha = 1))
  models[[fold]][["lasso"]] <- lasso
  runtimes[[fold]][["lasso"]] <- lasso_time
  cv.mse[[fold]][["lasso"]] <- calc_mse(lasso, test_dat)
  
  # Ridge
  ridge_time <- system.time(ridge <- cv.glmnet(x = as.matrix(train_dat[,-1]), y = as.matrix(train_dat[,1]), alpha = 0))
  models[[fold]][["ridge"]] <- ridge
  runtimes[[fold]][["ridge"]] <- ridge_time
  cv.mse[[fold]][["ridge"]] <- calc_mse(ridge, test_dat)
  
  # Elastic net
  enet_time <- system.time(enet <- cv.glmnet(x = as.matrix(train_dat[,-1]), y = as.matrix(train_dat[,1]), alpha = 0.5))
  models[[fold]][["enet"]] <- enet
  runtimes[[fold]][["enet"]] <- enet_time
  cv.mse[[fold]][["enet"]] <- calc_mse(enet, test_dat)
  
  # Adaptive Lasso
  adap_lasso_time <- system.time(adap_lasso <- cv.gcdnet(x = as.matrix(train_dat[,-1]),
                                                         y = train_dat[,1], nfolds = 10,
                                                         method = "ls", lambda2 = 0))
  models[[fold]][["adap_lasso"]] <- adap_lasso
  runtimes[[fold]][["adap_lasso"]] <- adap_lasso_time
  cv.mse[[fold]][["adap_lasso"]] <- calc_mse(adap_lasso, test_dat)
  
  # Adaptive Ridge
  adap_ridge_time <- system.time(adap_ridge <- cv.gcdnet(x = as.matrix(train_dat[,-1]),
                                                         y = train_dat[,1], nfolds = 10,
                                                         method = "ls", lambda = 0))
  models[[fold]][["adap_ridge"]] <- adap_ridge
  runtimes[[fold]][["adap_ridge"]] <- adap_ridge_time
  cv.mse[[fold]][["adap_ridge"]] <- calc_mse(adap_ridge, test_dat)
  
  # Adaptive Elastic Net
  adap_enet_time <- system.time(adap_enet <- cv.gcdnet(x = as.matrix(train_dat[,-1]),
                                                       y = train_dat[,1], nfolds = 10,
                                                       method = "ls"))
  models[[fold]][["adap_enet"]] <- adap_enet
  runtimes[[fold]][["adap_enet"]] <- adap_enet_time
  cv.mse[[fold]][["adap_enet"]] <- calc_mse(adap_enet, test_dat)
  
  # SCAD
  scad_time <- system.time(scad <- cv.ncvreg(X = as.matrix(train_dat[, -1]), y = as.matrix(train_dat[,1]), penalty = "SCAD"))
  models[[fold]][["scad"]] <- scad
  runtimes[[fold]][["scad"]] <- scad_time
  cv.mse[[fold]][["scad"]] <- calc_mse(scad, test_dat)
  
  # MCP
  mcp_time <-  system.time(mcp <- cv.ncvreg(X = as.matrix(train_dat[, -1]), y = as.matrix(train_dat[,1])))
  models[[fold]][["mcp"]] <- mcp
  runtimes[[fold]][["mcp"]] <- mcp_time
  cv.mse[[fold]][["mcp"]] <- calc_mse(mcp, test_dat)
  
}

# create dataframe of cross validation values
mse_df <- data.frame(t(matrix(unlist(cv.mse), nrow=length(cv.mse), byrow=TRUE)))
avg_mse <- rowMeans(mse_df)
mse_df <- cbind(mse_df, avg_mse)
row.names(mse_df) <- c("lasso", "ridge", "enet", "adap lasso", "adap ridge", "adap enet", "scad", "mcp")