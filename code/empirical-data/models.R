# Analysis of bcTCGA breast cancer gene expression data

library(glmnet)
library(gcdnet)
library(ncvreg)
library(ranger)
library(randomForest)
library(xgboost)
library(e1071)
library(caret) #for cross validation data splitting
library(tidyverse) #data cleaning

# prevent stack overflow
options(expressions = 5e5)
memory.limit(size=8000000)

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

# Create histogram of y data
# Histogram
library(ggplot2)
p<-ggplot(cancer_df, aes(x=y)) + 
  geom_histogram(color="black", fill="tomato", binwidth = 0.25) 
# Add mean line
p <- p+ geom_vline(aes(xintercept=mean(y)),
              color="black", linetype="dashed", size=1)
p <- p + labs(x="BRCA1 Gene Expression")
p

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
  
  # XGBoost Grid Search
  # Separate data into x and y matrices. This is needed to run xgboost.
  train_x_data <- as.matrix(train_dat[, -1])
  train_y_data <- as.matrix(train_dat[, 1])
  
  xgb_time <- system.time({
    xgb_hyper_grid <- expand.grid(
      eta = c(.1, .3, .5),  # learning rate
      max_depth = c(1, 3, 7), # maximum depth of each tree
      optimal_trees = 0,               # a place to dump results
      min_RMSE = 0                     # a place to dump results
    )
    
    # lapply(1:nrow(xgb_hyper_grid), function(i) {
    #   # create parameter list
    #   params <- list(
    #     eta = xgb_hyper_grid$eta[i],
    #     max_depth = xgb_hyper_grid$max_depth[i],
    #     min_child_weight = xgb_hyper_grid$min_child_weight[i],
    #     subsample = xgb_hyper_grid$subsample[i],
    #     colsample_bytree = xgb_hyper_grid$colsample_bytree[i]
    #   )
    # 
    #   # train model
    #   xgb.tune <- xgb.cv(
    #     params = params,
    #     data = train_x_data,
    #     label = train_y_data,
    #     nrounds = 1000,
    #     nfold = 5,
    #     objective = "reg:squarederror",  # for regression models
    #     verbose = 0,               # silent,
    #     early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
    #   )
    # 
    #   # add min training error and trees to grid
    #   xgb_hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
    #   xgb_hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
    # })
    
    # grid search
    for(i in 1:nrow(xgb_hyper_grid)) {
      
      # create parameter list
      params <- list(
        eta = xgb_hyper_grid$eta[i],
        max_depth = xgb_hyper_grid$max_depth[i]
      )
      
      # train model
      xgb.tune <- xgb.cv(
        params = params,
        min_child_weight = 1, # default
        subsample = 1, 
        colsample_bytree = 1,
        data = train_x_data,
        label = train_y_data,
        nrounds = 1000,
        nfold = 5,
        objective = "reg:squarederror",  # for regression models
        verbose = 0,               # silent,
        early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
      )
      
      # add min training error and trees to grid
      xgb_hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
      xgb_hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
    }
    
    xgb_best_grid <- xgb_hyper_grid %>%
      dplyr::arrange(min_RMSE)
    
    xgb_best_params <- list(
      eta = xgb_best_grid$eta[1],
      max_depth = xgb_best_grid$max_depth[1]
    )
    
    train_set <- xgb.DMatrix(data = as.matrix(train_dat[, -1]), label = as.matrix(train_dat[, 1]))
    
    # train best model
    xgb.best <- xgboost(
      params = xgb_best_params,
      data = train_set,
      nrounds = 1000,
      objective = "reg:squarederror",  # for regression models
      verbose = 0,               # silent,
      early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
    )
  })
  models[[fold]][["gbm"]] <- xgb.best
  runtimes[[fold]][["gbm"]] <- xgb_time
  cv.mse[[fold]][["gbm"]] <- calc_mse(xgb.best, test_dat)
  
  # Random Forest Grid Search
  rf_time <- system.time({
    p <- length(train_dat)
    
    rf_hyper_grid <- expand.grid(
      mtry       = c(floor(sqrt(p)), floor(p / 3), floor(p/2)),  # Predictors per tree
      n.trees    = c(300, 500, 700),
      OOB_RMSE   = 0
    )
    
    # lapply(1:nrow(rf_hyper_grid), function(i) {
    #   # train model
    #   rf_model <- ranger(
    #     formula         = y ~ ., 
    #     data            = dat, 
    #     num.trees       = rf_hyper_grid$n.trees[i],
    #     mtry            = rf_hyper_grid$mtry[i],
    #     min.node.size   = rf_hyper_grid$node_size[i],
    #     sample.fraction = rf_hyper_grid$sampe_size[i],
    #     seed            = 123
    #   )
    #   
    #   # add OOB error to grid
    #   rf_hyper_grid$OOB_RMSE[i] <- sqrt(rf_model$prediction.error)
    # })
    
    for(i in 1:nrow(rf_hyper_grid)) {
      
      # train model
      rf_model <- randomForest(
        x               = train_dat[,-1],
        y               = train_dat[,1],
        ntree           = rf_hyper_grid$n.trees[i],
        mtry            = rf_hyper_grid$mtry[i],
        nodesize        = 5
      )
      
      # add OOB error to grid
      rf_hyper_grid$OOB_RMSE[i] <- sqrt(mean(rf_model$mse))
    }
    
    rf_best_grid <- rf_hyper_grid %>% 
      dplyr::arrange(OOB_RMSE)
    
    # train best model
    best_rf_model <- randomForest(
      x               = train_dat[,-1],
      y               = train_dat[,1], 
      ntree           = rf_best_grid$n.trees[1],
      mtry            = rf_best_grid$mtry[1],
      nodesize        = 5
    )
  })
  models[[fold]][["rf"]] <- best_rf_model
  runtimes[[fold]][["rf"]] <- rf_time
  cv.mse[[fold]][["rf"]] <- calc_mse(best_rf_model, test_dat)
  
  # # Support Vector Machine
  # svm_time <- system.time({
  #   svm_tune <- tune.svm(y ~ ., data = train_dat,
  #                        epsilon = seq(0.1, 0.5, 0.2),
  #                        cost = c(0.5, 1, 2), kernel = "linear"
  #   )
  # 
  #   svm_model <- svm_tune$best.model
  # })
  # 
  # models[[fold]][["svm"]] <- svm_model
  # runtimes[[fold]][["svm"]] <- svm_time
  # cv.mse[[fold]][["svm"]] <- calc_mse(svm_model, test_dat)
}

# create dataframe of cross validation values
mse_df <- data.frame(t(matrix(unlist(cv.mse), nrow=length(cv.mse), byrow=TRUE)))
avg_mse <- rowMeans(mse_df)
mse_df <- cbind(mse_df, avg_mse)
row.names(mse_df) <- c("lasso", "ridge", "enet", "adap lasso", "adap ridge", "adap enet", "scad", "mcp", "xgboost", "rf")