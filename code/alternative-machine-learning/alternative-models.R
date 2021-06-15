# Script with alternative machine learning regression models
#     - Random Forest
#     - Gradient Boosting Model
#     - XGBoost
#     - Support Vector Machine

# import required packages
library(MASS) #includes databases
library(randomForest) #Random Forest model
library(gbm) #gradient boosting model
library(xgboost) #xgboost model
library(e1071) #support vector machine model


set.seed(23122) #set seed to keep consistent results

nrows <- nrow(Boston)
train <- sample(1:nrow(Boston),size=round(nrows*.75))  #seperate 75% into training data

# Separating data
train_x_data <-  as.matrix(Boston[train, 1:length(Boston)-1])  #training x data
train_y_data <- as.matrix(Boston[train, "medv"])  #training y data (AKA labels)
test_x_data <- as.matrix(Boston[-train, 1:length(Boston)-1])
test_y_data <- Boston[-train, "medv"]


# Random Forest Model Regression
rf <- randomForest(medv ~ ., data = Boston[train,])
rf_mse <- mean((test_y_data - predict(rf, test_x_data)) ^ 2)

# Gradient Boosting Model Regression
gbm_boost <- gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
#summary(gbm_boost) #gives a table of Variable Importance and a plot of Variable Importance
gbm_mse <- mean((test_y_data - predict(gbm_boost, as.data.frame(test_x_data))) ^ 2)

# XGBoost Model using Tree Boosting
xgb_tree <- xgboost(data = train_x_data, label = train_y_data,
                     booster = "gbtree", #use devision trees
                     nrounds = 1000,  #number of boosting iterations
                     objective = "reg:squarederror",  #regression by RSS
                     early_stopping_rounds = 3,  #stops boosting early if mse doesnt improve after a certain number of rounds
                     max_depth = 6,  #maximum depth of the tree
                     eta = .25,  #determines learning rate. Lower: less overfitting, but more nrounds and slower computation
                     verbose = 0)  #whether to print information during boosting: 0 = nothing, 1 = some info, 2 = more info

xgb_tree_train_mse <- mean((test_y_data - predict(xgb_tree, test_x_data)) ^ 2)

# XGBoost Model using Linear Boosting
xgb_linear <- xgboost(data = train_x_data, label = train_y_data,
                    booster = "gblinear", #use linear models
                    nrounds = 10000,  #number of boosting iterations
                    objective = "reg:squarederror",  #regression by RSS
                    early_stopping_rounds = 3,  #stops boosting early if mse doesnt improve after a certain number of rounds
                    lambda = 0, #L2 regularization term on weights (ridge regression tuning parameter)
                    alpha = 0, #L1 regularization term on weights (lasso regression tuning parameter)
                    verbose = 0)  #whether to print information during boosting: 0 = nothing, 1 = some info, 2 = more info

xgb_linear_train_mse <- mean((test_y_data - predict(xgb_linear, test_x_data)) ^ 2)

# Support Vector Machine regression
svm_model <- svm(medv ~ ., data = Boston[train, ])
svm_mse <- mean((test_y_data - predict(svm_model, test_x_data)) ^ 2)
