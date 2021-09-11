rm(list = ls())

library(ggplot2)
library(broom)

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

load("../empirical-data/simulation-environment.RData")

library(ggplot2)
library(glmnet)
library(gcdnet)
library(ncvreg)
library(ranger)
library(randomForest)
library(xgboost)
library(tables)

# calc_mse copied from models.R in empirical-data.
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

metrics_by_fold <- lapply(1:5, function(fold) {
  train_dat <- cancer_df[-cv_folds[[fold]],]
  test_dat <- cancer_df[cv_folds[[fold]],]
  
  ridge <- models[[fold]]$ridge
  lasso <- models[[fold]]$lasso
  enet <- models[[fold]]$enet
  scad <- models[[fold]]$scad
  mcp <- models[[fold]]$mcp
  gbm <- models[[fold]]$gbm
  rf <- models[[fold]]$rf
  
  models <- list(ridge, lasso, enet, scad, mcp, gbm, rf)
  
  model_name <- factor(c("Ridge", "Lasso", "E-net", "SCAD", "MCP", "XGBoost", "RF"))
  train_mse <- as.numeric(lapply(models, calc_mse, dat = train_dat))
  test_mse <- as.numeric(lapply(models, calc_mse, dat = test_dat))
  
  test_metrics <- data.frame(model_name = model_name,
                             type = "Mean Test MSE",
                             mse = test_mse)
  
  train_metrics <- data.frame(model_name = model_name,
                              type = "Mean Train MSE",
                              mse = train_mse)
  
  all_metrics <- rbind(test_metrics, train_metrics)
  all_metrics$model_name <- factor(all_metrics$model_name, levels = model_name)
  return(all_metrics)
})

all_results <- do.call(rbind, metrics_by_fold)
all_results$type <- factor(all_results$type, levels = c("Mean Train MSE", "Mean Test MSE"))

aggregate <- aggregate(mse ~ model_name + type, data = all_results, mean)

mse_plot <- ggplot(data = all_results, mapping = aes(x = model_name, y = mse, label = mse)) +
  geom_bar(mapping = aes(fill = type), stat = "summary", fun = "mean", position = position_dodge(width = 0.8), width = 0.8, color = "black") +
  geom_point(mapping = aes(group = type), position = position_dodge(width = 0.8)) +
  #geom_text(data = aggregate, aes(label = round(mse, 4), group = type), position = position_dodge(width = 0.8), vjust=-0.25) +
  labs(x = "Model name", y = "Mean Squared Error") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  reu_border +
  theme(
    panel.grid = element_line(color = "gray90"),
    legend.title=element_blank(),
    legend.spacing.y = unit(1.0, "cm")
  )

save_plot(plot = mse_plot,
          filename = "empirical_mse",
          path = "../../figures")

tab <- tabular(model_name ~ (type * mse) * (mean + sd), data = all_results)
print(toLatex(tab, options = list(justification = "l")))
