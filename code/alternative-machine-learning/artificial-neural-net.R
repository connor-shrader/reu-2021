# Details and trains Artificial Neural Network models
#    -Changes ANN model structures
#    -Compares MSE to Random Forest MSE

# import required packages
library(MASS) #includes databases
library(tidyverse) #data cleaning
library(neuralnet) #neural network model


dat <- Boston

# Scale data from 0-1 (necessary for ANNs)
scale01 <- function(x){
  new_dat <- (x - min(x)) / (max(x) - min(x))
  return(new_dat)
}

dat <- dat %>%
  mutate_all(scale01)

set.seed(12345) #set seed for consistency

# Split into test and train sets
nrows <-  nrow(dat)
train <- sample(1:nrows,size=round(nrows*.75)) #75% training data
data_train <- dat[train,]
data_test <- dat[-train,]

# 1 Layer, 1 Neuron Neural Net
boston_ANN1 <- neuralnet(medv ~ ., data = data_train, hidden = 1)
ANN1_mse <- mean((data_test[,"medv"] - predict(boston_ANN1, data_test[,-length(data_test)])) ^ 2)
#plot(boston_ANN1, rep = "best")  #view plot of neural network structure

# 1 Layer, 3 Neuron Neural Net
boston_ANN2 <- neuralnet(medv ~ ., data = data_train, hidden = 3)
ANN2_mse <- mean((data_test[,"medv"] - predict(boston_ANN2, data_test[,-length(data_test)])) ^ 2)

# 3 Layer, 9 Neuron Neural Net
boston_ANN3 <- neuralnet(medv ~ ., data = data_train, hidden = c(3,3,3))
ANN3_mse <- mean((data_test[,"medv"] - predict(boston_ANN3, data_test[,-length(data_test)])) ^ 2)

# 4 Layer, 14 Neuron Neural Net
boston_ANN4 <- neuralnet(medv ~ ., data = data_train, hidden = c(5,5,3,1))
ANN4_mse <- mean((data_test[,"medv"] - predict(boston_ANN4, data_test[,-length(data_test)])) ^ 2)


# Comparison with RandomForest model
rf <- randomForest(medv ~ ., data = data_train)
rf_mse <- mean((data_test[,"medv"] - predict(rf, data_test[,-length(data_test)])) ^ 2)