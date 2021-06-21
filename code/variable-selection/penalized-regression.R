


#setup
library(MASS) #used for Boston dataframe
library(gcdnet) #used for adaptive elastic net regression
library(glmnet) #used for lasso regression
library(tidyverse) #data cleaning and analysis


set.seed(23122) #set seed to keep consistent results

nrows <- nrow(Boston)
train <- sample(1:nrow(Boston),size=round(nrows*.75))  #seperate 75% into training data

######Separating data#####
train_x_data <-  as.matrix(Boston[train, 1:length(Boston)-1])  #training x data
train_y_data <- as.matrix(Boston[train, "medv"])  #training y data (AKA labels)
test_x_data <- as.matrix(Boston[-train, 1:length(Boston)-1])
test_y_data <- Boston[-train, "medv"]

adap_enet <- cv.gcdnet(x = train_x_data, y = train_y_data, nfolds = 10, method = "ls")
adap_enet_yhat <- predict(adap_enet, newx = test_x_data)
adap_enet_mse <- mean((adap_enet_yhat - test_y_data)^2)


lasso <- cv.glmnet(x = train_x_data, y = train_y_data, alpha = 1)
lasso_yhat <- predict(lasso, newx = test_x_data)
lasso_mse <- mean((lasso_yhat - test_y_data)^2)
