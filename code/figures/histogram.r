# Used for figures of empirical data and results
# Make sure to import empirical results environment beforehand

library(caret) #for cross validation data splitting
library(tidyverse) #data cleaning
library(ggplot2)
library(scales)


# load data and set it up into a dataframe
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
source("save-plot.r")

cancer <- readRDS("../empirical-data/bcTCGA.rds")
cancer_y <- data.frame(y = cancer[["y"]])

# Create histogram of y data
# Histogram
y_hist <- ggplot(cancer_y, aes(x = y)) + 
  geom_histogram(color = "black", fill = hue_pal()(2)[1], binwidth = 0.25) + 
  
  # Mean line
  geom_vline(aes(xintercept=mean(y)), color = "black", linetype="dashed", size=1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x="BRCA1 Gene Expression", y = "Count") +
  reu_border +
  theme(
    panel.grid = element_line(color = "gray90")
  )

print(mean(cancer_y$y))

save_plot(plot = y_hist,
          filename = "empirical_histogram",
          path = "./images")
