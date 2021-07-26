# Goal of script is to explore classification methods.
#   - Examine logistic regression (classification) methods and models
#   - Examine alternative classification methods and models
# This did not end up being used in the study


#setup
library(MASS) #used for cats dataframe and subset selection
library(gcdnet) #used for adaptive elastic net regression
library(glmnet) #used for lasso regression
library(tidyverse) #data cleaning and analysis
library(ncvreg) #used for MCP and SCAD models
library(xgboost) #used for training xgboost models
library(ranger) #used for fast training of randomForest models
library(e1071) #used for svm


set.seed(23122) #set seed to keep consistent results

nrows <- nrow(cats)
# Seperating data
train <- sample(1:nrow(cats),size=round(nrows*.75))  #seperate 75% into training data
dat <- cats[train,]


# full model
fm <- glm(Sex ~ ., data = dat, family = "binomial")
fm_pred <- predict(fm, cats[-train,], type = "response")
fm_pred_classes <- ifelse(fm_pred > 0.5, "M", "F")
fm_accuracy <- mean(fm_pred_classes==cats[-train, "Sex"])

# null model
nm <- glm(Sex ~ 1 , data = dat, family = binomial)

# AIC and BIC model selection for backward
ab <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
               direction="backward", k=2, trace=F,
               steps=3000)

bb <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
               direction="backward", k=log(nrow(dat)),
               trace=F, steps=3000)

# AIC and BIC model selection for stepwise backward
asb <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
                direction="both", k=2, trace=F,
                steps=3000)

bsb <-  stepAIC(fm, scope=list(lower=nm, upper=fm),
                direction="both", k=log(nrow(dat)),
                trace=F, steps=3000)

# AIC and BIC model for forward
af <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
               direction="forward", k=2, trace=F,
               steps=3000)

bf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
               direction="forward", k=log(nrow(dat)),
               trace=F, steps=3000)

# AIC and BIC model selection for stepwise forward
asf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
                direction="both", k=2, trace=F,
                steps=3000)

bsf <-  stepAIC(nm, scope=list(lower=nm, upper=fm),
                direction="both", k=log(nrow(dat)),
                trace=F, steps=3000)

# Ridge classification
ridge <- cv.glmnet(x = as.matrix(dat[,-1]),
                   y = dat$Sex, alpha = 0, family = "binomial")
ridge_classes <- predict(ridge, as.matrix(cats[-train, -1]), type = "class")
ridge_accuracy <- mean(ridge_classes == cats[-train, "Sex"])

# Lasso classification
lasso <- cv.glmnet(x = as.matrix(dat[,-1]), y = dat$Sex, alpha = 1, family = "binomial")
lasso_classest <- predict(lasso, as.matrix(cats[-train, -1]), type = "class")

# Elastic Net
enet <- cv.glmnet(x = as.matrix(dat[,-1]), 
                  y = dat$Sex, alpha = 0.8, family = "binomial")
enet_classes <- predict(enet, as.matrix(cats[-train, -1]), type = "class")

# Adaptive Ridge
adap_ridge <- cv.gcdnet(x = as.matrix(dat[,-1]),
                        y = dat$Sex, nfolds = 10,
                        method = "logit", lambda = 0)
adap_ridge_yhat <- predict(adap_ridge, as.matrix(cats[-train, -1]), type = "class")
adap_ridge_classes <- ifelse(adap_ridge_yhat > 0, "M", "F")

# Adaptive Lasso
adap_lasso <- cv.gcdnet(x = as.matrix(dat[,-1]),
                        y = dat$Sex, nfolds = 10,
                        method = "logit", lambda2 = 0)
adap_lasso_yhat <- predict(adap_lasso, as.matrix(cats[-train, -1]), type = "class")
adap_lasso_classes <- ifelse(adap_lasso_yhat > 0, "M", "F")

# Adaptive Elastic Net
adap_enet <- cv.gcdnet(x = as.matrix(dat[,-1]), y = dat$Sex, nfolds = 10, method = "logit")
adap_enet_yhat <- predict(adap_enet, as.matrix(cats[-train, -1]), type = "class")
adap_enet_classes <- ifelse(adap_enet_yhat > 0, "M", "F")

# MCP
mcp <- cv.ncvreg(X = dat[, -1], y = dat$Sex, family = "binomial")
mcp_yhat <- predict(mcp, as.matrix(cats[-train, -1]), type = "class")
mcp_classes <- ifelse(mcp_yhat > 0, "M", "F")

# SCAD
scad <- cv.ncvreg(X = dat[, -1], y = dat$Sex, penalty = "SCAD", family = "binomial")
scad_yhat <- predict(scad, as.matrix(cats[-train, -1]), type = "class")
scad_classes <- ifelse(scad_yhat > 0, "M", "F")

##XGBoost##
# convert labels to 0 and 1s
train_labels <- ifelse(dat[, "Sex"] == "F", 0, 1)
test_labels <- ifelse(cats[-train, "Sex"] == "F", 0, 1)

xgb_hyper_grid <- expand.grid(
  eta = c(.1, .3, .5),  # learning rate
  max_depth = c(1, 3, 7), # maximum depth of each tree
  optimal_trees = 0,               # a place to dump results
  min_loss = 0                     # a place to dump results
)

# lapply(1:nrow(xgb_hyper_grid), function(i) {
#   # create parameter list
#   params <- list(
#     eta = xgb_hyper_grid$eta[i],
#     max_depth = xgb_hyper_grid$max_depth[i],
#     min_child_weight = xgb_hyper_grid$min_child_weight[xgb_i],
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
#     objective = "reg:squarederror",  # for classification models
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
    min_child_weight = 1, 
    subsample = 1, 
    colsample_bytree = 1,
    data = as.matrix(dat[,-1]),
    label = as.matrix(train_labels),
    nrounds = 1000,
    nfold = 5,
    objective = "binary:logistic",  # for classification models
    eval_metric = "logloss", #for finding logit loss
    verbose = 0,               # silent,
    early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  xgb_hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_logloss_mean)
  xgb_hyper_grid$min_loss[i] <- min(xgb.tune$evaluation_log$test_logloss_mean)
}

xgb_best_grid <- xgb_hyper_grid %>%
  dplyr::arrange(min_loss)

xgb_best_params <- list(
  eta = xgb_best_grid$eta[1],
  max_depth = xgb_best_grid$max_depth[1]
)

# train best model
xgb.best <- xgboost(
  params = xgb_best_params,
  data = as.matrix(dat[,-1]),
  label = as.matrix(train_labels),
  nrounds = 1000,
  objective = "binary:logistic",  # for classification models
  eval_metric = "logloss", #for finding logit loss
  verbose = 0,               # silent,
  early_stopping_rounds = 10, # stop if no improvement for 10 consecutive trees
)

xgb_yhat <- predict(xgb.best, as.matrix(cats[-train, -1]))
xgb_classes <- ifelse(xgb_yhat > 0.5, "M", "F")
xgb_acc <- mean(xgb_classes == cats[-train, "Sex"])



##Ranger Random Forest##
p = length(cats)

rf_hyper_grid <- expand.grid(
  mtry       = c(floor(sqrt(p)), floor(p / 3), floor(p / 2)),  # Predictors per tree
  n.trees    = c(300, 400, 500, 600),
  OOB_loss   = 0
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
#   rf_hyper_grid$OOB_loss[i] <- sqrt(rf_model$prediction.error)
# })

for(i in 1:nrow(rf_hyper_grid)) {
  
  # train model
  rf_model <- ranger(
    formula         = Sex ~ .,
    data            = dat,
    num.trees       = rf_hyper_grid$n.trees[i],
    mtry            = rf_hyper_grid$mtry[i],
    min.node.size   = 5,
    seed            = 123
  )
  
  # add OOB error to grid
  rf_hyper_grid$OOB_loss[i] <- sqrt(rf_model$prediction.error)
}

rf_best_grid <- rf_hyper_grid %>% 
  dplyr::arrange(OOB_loss)

# train best model
best_rf_model <- ranger(
  formula         = Sex ~ ., 
  data            = dat, 
  num.trees       = rf_best_grid$n.trees[1],
  mtry            = rf_best_grid$mtry[1],
  min.node.size   = 5,
  seed            = 123
)

rf_classes <- predict(best_rf_model, cats[-train, -1])$predictions
rf_acc <- mean(rf_classes == cats[-train, "Sex"])

##SVM Model##
svm_tune <- tune.svm(Sex ~ ., data = dat,
                     epsilon = seq(0.1, 0.5, 0.2),
                     cost = c(0.5, 1, 2)
)

svm_model <- svm_tune$best.model #find best model

svm_classes <- predict(svm_model, cats[-train, -1])
svm_acc <- mean(svm_classes == cats[-train, "Sex"])
