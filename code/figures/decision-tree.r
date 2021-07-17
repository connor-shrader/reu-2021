library(ggplot2)
library(rstudioapi)
library(faux)
library(rpart)
library(rpart.plot)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

set.seed(1)
data(iris)
fit <- rpart(Species ~ ., data = iris, method = "class")

setEPS()
postscript("images/decision-tree.eps")
rpart.plot(fit, type = 5, extra = 0, legend.y = -1, cex = 1.8)
dev.off()

png(file = "images/decision-tree.png")
rpart.plot(fit, type = 5, extra = 0, legend.y = -1, cex = 1.8)
dev.off()
