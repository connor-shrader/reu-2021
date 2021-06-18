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
library(h2o) #grid search and optimization


set.seed(23122) #set seed to keep consistent results

nrows <- nrow(Boston)
train <- sample(1:nrow(Boston),size=round(nrows*.75))  #seperate 75% into training data

######Separating data#####
train_x_data <-  as.matrix(Boston[train, 1:length(Boston)-1])  #training x data
train_y_data <- as.matrix(Boston[train, "medv"])  #training y data (AKA labels)
test_x_data <- as.matrix(Boston[-train, 1:length(Boston)-1])
test_y_data <- Boston[-train, "medv"]


###### Random Forest Model Regression #####
rf <- randomForest(medv ~ ., data = Boston[train,])
rf_mse <- mean((test_y_data - predict(rf, test_x_data)) ^ 2)

#### Gradient Boosting Model Regression #####
#basic GBM
gbm_boost <- gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
#summary(gbm_boost) #gives a table of Variable Importance and a plot of Variable Importance
gbm_mse <- mean((test_y_data - predict(gbm_boost, as.data.frame(test_x_data))) ^ 2)

# XGBoost Model using Tree Boosting
xgb_tree <- xgboost(data = train_x_data, label = train_y_data,
                     booster = "gbtree", #use decision trees
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


##### Support Vector Machine regression #####
svm_model <- svm(medv ~ ., data = Boston[train, ])
svm_mse <- mean((test_y_data - predict(svm_model, test_x_data)) ^ 2)

##SVM grid search code##
svm_fit <- function(kernel, equation, data){
  svm_mod <- svm(equation, data = data, kernel = kernel) #fit svm model
  return(svm_mod)
}

calc_mse <- function(model, test_data){
  pred_columns <- attr(model$terms, "term.labels") #get list of data columns
  test_x <- test_data[,pred_columns] #separate test x and y data
  test_y <- as.vector(test_data[[toString(as.list(svm_model$terms)[[2]])]]) #retrieve y column from model and then retrieve y data
  predicted_y <- as.vector(predict(model, newdata = test_x)) #predict new y values
  
  mse <- mean((test_y - predicted_y) ^ 2) #calculate mse
  return(mse)
}

svm_grid <- function(equation, data, test_data){
  kernels <-  c("linear", "polynomial", "radial")
  models <- lapply(kernels, svm_fit, equation = equation, data = data) #generate list of models
  mse_list <- lapply(models, calc_mse, test_data = test_data) #calculate mse for each model
  min_index <- which.min(mse_list) #find min mse and return it
  return(models[[min_index]])
}


grid_svm_model <- svm_grid(medv~., data = Boston[train,], test_data = Boston[-train, ]) #returns grid search model


#### Using h2o library####
h2o.no_progress()
h2o.init(max_mem_size = "8g")  #start up h2o

##GDM H2O##
# create hyperparameter grid
hyper_grid <- list(
  max_depth = c(1, 3, 5),
  min_rows = c(1, 5, 10)#,
  #learn_rate = c(0.01, 0.05, 0.1),
  #learn_rate_annealing = c(.99, 1),
  #sample_rate = c(.5, .75, 1),
  #col_sample_rate = c(.8, .9, 1)
)

gdm_search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 60*60
)

# perform grid search for gdm
gdm_grid <- h2o.grid(
  algorithm = "gbm",
  y = "medv", 
  training_frame = as.h2o(Boston[train, ]),
  validation_frame = as.h2o(Boston[-train,]),
  hyper_params = hyper_grid,
  search_criteria = gdm_search_criteria,
  ntrees = 5000,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)

# collect the results and sort by our model performance metric of choice
gdm_grid_perf <- h2o.getGrid(
  grid_id = gdm_grid@grid_id, 
  sort_by = "mse", 
  decreasing = FALSE
)

#gdm_grid_perf

# Grab the model_id for the top model, chosen by validation error
gdm_best_model_id <- gdm_grid_perf@model_ids[[1]]
gdm_best_model <- h2o.getModel(gdm_best_model_id)

# performance metrics on the best model
gdm_best_model_perf <- h2o.performance(model = gdm_best_model, valid = TRUE, newdata = as.h2o(Boston[-train,]))



##Random forest H2O##
num_p <- length(Boston[train,]) - 1

rf_grid.h2o <- list(
  ntrees      = c(200, 500, 1500),
  mtries      = c(round(num_p /2), round(num_p / 3))#,
  #max_depth   = c(20, 30, 40),
  #sample_rate = c(.55, .632, .75)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 30*60
)

# build grid search 
rf_grid <- h2o.grid(
  algorithm = "randomForest",
  y = "medv", 
  training_frame = as.h2o(Boston[train, ]),
  hyper_params = rf_grid.h2o,
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice
rf_grid_perf <- h2o.getGrid(
  grid_id = rf_grid@grid_id, 
  sort_by = "mse", 
  decreasing = FALSE
)

# Grab the model_id for the top model, chosen by validation error
rf_best_model_id <- rf_grid_perf@model_ids[[1]]
rf_best_model <- h2o.getModel(rf_best_model_id)

# evaluate the model performance on a test set
rf_best_model_perf <- h2o.performance(model = rf_best_model, newdata = as.h2o(Boston[-train,]))
