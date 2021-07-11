rm(list = ls())

library(ggplot2)
library(broom)

library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")
load("../empirical-data/simulation-environment.RData")

# https://stackoverflow.com/questions/36656752/plotting-cv-glmnet-in-r

# library(ggrepel)

tidied_ridge <- tidy(ridge)
tidied_adap_ridge <- data.frame(lambda = adap_ridge$lambda,
                               estimate = adap_ridge$cvm,
                               std.error = adap_ridge$cvsd)
tidied_adap_ridge$conf.low <- tidied_adap_ridge$estimate - tidied_adap_ridge$std.error
tidied_adap_ridge$conf.high <- tidied_adap_ridge$estimate + tidied_adap_ridge$std.error
tidied_adap_ridge$nzero <- adap_ridge$nzero

tidied_lasso <- tidy(lasso)
tidied_lasso$model_name <- "Lasso"

tidied_enet <- tidy(enet)
tidied_enet$model_name <- "E-net"

tidied_adap_lasso <- data.frame(lambda = adap_lasso$lambda,
                                estimate = adap_lasso$cvm,
                                std.error = adap_lasso$cvsd)
tidied_adap_lasso$conf.low <- tidied_adap_lasso$estimate - tidied_adap_lasso$std.error
tidied_adap_lasso$conf.high <- tidied_adap_lasso$estimate + tidied_adap_lasso$std.error
tidied_adap_lasso$nzero <- adap_lasso$nzero
tidied_adap_lasso$model_name <- "Adap. Lasso"

tidied_adap_enet <- data.frame(lambda = adap_enet$lambda,
                                estimate = adap_enet$cvm,
                                std.error = adap_enet$cvsd)
tidied_adap_enet$conf.low <- tidied_adap_enet$estimate - tidied_adap_enet$std.error
tidied_adap_enet$conf.high <- tidied_adap_enet$estimate + tidied_adap_enet$std.error
tidied_adap_enet$nzero <- adap_enet$nzero
tidied_adap_enet$model_name <- "Adap. e-net"

tidied_scad <- data.frame(lambda = scad$lambda,
                          estimate = scad$cve,
                          std.error = scad$cvse)
tidied_scad$conf.low <- tidied_scad$estimate - tidied_scad$std.error
tidied_scad$conf.high <- tidied_scad$estimate + tidied_scad$std.error
tidied_scad$nzero <- colSums(scad$fit$beta != 0) - 1
tidied_scad$model_name <- "SCAD"

tidied_mcp <- data.frame(lambda = mcp$lambda,
                         estimate = mcp$cve,
                         std.error = mcp$cvse)
tidied_mcp$conf.low <- tidied_mcp$estimate - tidied_mcp$std.error
tidied_mcp$conf.high <- tidied_mcp$estimate + tidied_mcp$std.error
tidied_mcp$nzero <- colSums(mcp$fit$beta != 0) - 1
tidied_mcp$model_name <- "MCP"

tidied_vs_info <- do.call("rbind", list(tidied_lasso, tidied_enet, tidied_adap_lasso,
                                     tidied_adap_enet, tidied_scad, tidied_mcp))
vs_model_names <- c("Lasso", "E-net", "Adap. Lasso", "Adap. e-net", "SCAD", "MCP")
tidied_vs_info$model_name <- factor(tidied_vs_info$model_name,
                                 levels = model_names)

best_vs_lambdas <- c(lasso$lambda.min, enet$lambda.min, adap_lasso$lambda.min,
                  adap_enet$lambda.min, scad$lambda.min, mcp$lambda.min)

best_vs_nzero <- c(tidied_lasso[tidied_lasso$lambda == best_vs_lambdas[1], "nzero"],
                tidied_enet[tidied_enet$lambda == best_vs_lambdas[2], "nzero"],
                tidied_adap_lasso[tidied_adap_lasso$lambda == best_vs_lambdas[3], "nzero"],
                tidied_adap_enet[tidied_adap_enet$lambda == best_vs_lambdas[4], "nzero"],
                tidied_scad[tidied_scad$lambda == best_vs_lambdas[5], "nzero"],
                tidied_mcp[tidied_mcp$lambda == best_vs_lambdas[6], "nzero"])

best_vs_values <- data.frame(model_name = model_names, lambda = best_vs_lambdas, nzero = as.numeric(best_vs_nzero))

nzero_plot <- ggplot(tidied_vs_info, aes(x = lambda, y = estimate)) +
  geom_line(mapping = aes(x = lambda, y = nzero, color = model_name)) +
  geom_point(data = best_values, mapping = aes(x = lambda, y = nzero, color = model_name), size = 3) +
  scale_x_log10() +
  labs(x = expression(lambda), y = "Number of Predictors", color = "Model", shape = "Correlation") +
  reu_border

mse_plot <- function(data) {
  ggplot(data, aes(x = lambda, y = estimate)) +
    geom_errorbar(mapping = aes(ymin = conf.low, ymax = conf.high), color = "gray") +
    geom_vline(xintercept = lasso$lambda.min, 
               linetype = "dashed",
               color = "red") +
    geom_point() +
    scale_x_log10() +
    labs(x = expression(lambda), y = "Mean Squared Error") +
    reu_border +
    theme(
      panel.grid = element_line(color = "gray90")
    )
}

lasso_mse <- mse_plot(tidied_lasso)

lasso_mse
