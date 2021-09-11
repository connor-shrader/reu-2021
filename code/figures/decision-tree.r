library(ggplot2)
library(rstudioapi)
library(faux)
library(rpart)
library(rpart.plot)
library(glmnet)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

set.seed(1)
load("../empirical-data/simulation-environment.RData")
lasso_coef <- models[[1]]$lasso %>% coef() %>% as.matrix()
lasso_coef[, 1] <- sort(lasso_coef[, 1], decreasing = TRUE)
var_names <- names(lasso_coef[2:5, 1])

cancer_df_mini <- cancer_df[, c("y", var_names)]

fit <- rpart(y ~ ., data = cancer_df_mini, method = "anova", maxdepth = 4)

setEPS()
postscript("../../figures/decision-tree.eps")
rpart.plot(fit, type = 0, extra = 1, legend.y = -1, cex = 1.4, digits = 3, gap = 2)
dev.off()

png(file = "../../figures/decision-tree.png")
rpart.plot(fit, type = 0, extra = 1, legend.y = -1, cex = 1, digits = 3, ygap = 0.1)
dev.off()
