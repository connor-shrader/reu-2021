# Used for figures of empirical data and results
# Make sure to import empirical results environment beforehand

library(caret) #for cross validation data splitting
library(tidyverse) #data cleaning
library(ggplot2)


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
y_hist<-ggplot(cancer_df, aes(x=y)) + 
  geom_histogram(color="black", fill="tomato", binwidth = 0.25) 
# Add mean line
y_hist <- y_hist+ geom_vline(aes(xintercept=mean(y)),
                   color="black", linetype="dashed", size=1)
y_hist <- y_hist + labs(x="BRCA1 Gene Expression")
y_hist


# Create bar plot of empirical data results
# Basic barplot
cv_plot_df <- mse_df
model_names <- c("Lasso", "Ridge", "Elas. Net", "Adap. Lasso", "Adap. Ridge", "Adap. Elas. Net", "SCAD", "MCP", "Random Forest", "XGBoost")
cv_plot_df$names <- factor(model_names, levels = model_names)
cv_plot_df$avg_mse <- round(cv_plot_df$avg_mse, 4)

cv_fig<-ggplot(data=cv_plot_df, aes(x=names, y=avg_mse)) +
  geom_bar(stat="identity", fill = "tomato") + 
  coord_cartesian(ylim=c(0.2,0.31)) + 
  geom_text(aes(label=avg_mse), vjust=-0.25) +
  labs(x = "Models", y = "Average Mean Square Error")
cv_fig
