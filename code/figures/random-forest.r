library(ggplot2)
library(rstudioapi)
library(faux)
library(randomForest)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

set.seed(1)
data(iris)
fit <- randomForest(Species ~ ., data = iris)
