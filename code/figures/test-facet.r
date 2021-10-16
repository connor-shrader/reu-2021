# Given plot_results

col_names <- names(plot_results)
col_names_kept <- col_names[col_names != "mean_train_mse" & col_names != "mean_test_mse"]

library(reshape2)

newdat <- melt(plot_results, id.vars = col_names_kept, value.name = "mean_mse")