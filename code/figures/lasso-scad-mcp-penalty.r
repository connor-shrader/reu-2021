library(ggplot2)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
source("themes.r")

len <- 500

# Parameter values used to compute LASSO, SCAD, and MCP.
lambda <- 2
a <- 3

# This function returns -1 if x is negative and 1 if x is positive.
sign <- function(x)
{
  return(ifelse(x < 0, -1, 1))
}

# PLOT 1: Penalty functions for LASSO, SCAD, and MCP.

# The following three functions compute the penalty for some coefficient value
# beta using the parameters lambda and a.

lasso <- function(beta, lambda)
{
  abs(beta)
}

scad <- function(beta, lambda, a)
{
  beta <- abs(beta)
  output <- ifelse (beta < lambda,  beta, 0)
  output <- ifelse(beta >= lambda & beta < a * lambda,
                   (2 * a * lambda * beta - beta^2 - lambda^2) / (2 * (a - 1) * lambda),
                   output)
  output <- ifelse(beta >= a * lambda, (lambda * (a + 1)) / 2, output)
  return(output)
}

mcp <- function(beta, lambda, a)
{
  beta <- abs(beta)
  output <- ifelse(beta < a * lambda, beta - (beta^2) / (2 * a), 0)
  output <- ifelse(beta >= a * lambda, 1/2 * a * lambda, output)
  return(output)
}

penalty <- ggplot() +
  geom_vline(
    data = data.frame(xint = c(-3, -1, 1, 3)),
    mapping = aes(xintercept = xint),
    linetype = "dashed",
    color = "gray"
  ) +
  
  geom_function(
    mapping = aes(color = "LASSO"),
    fun = lasso,
    args = list(lambda = 1),
    xlim = c(-4, 4)
  ) +
  
  geom_function(
    mapping = aes(color = "SCAD"),
    fun = scad,
    args = list(lambda = 1, a = 3),
    xlim = c(-4, 4)
  ) +
  
  geom_function(
    mapping = aes(color = "MCP"),
    fun = mcp,
    args = list(lambda = 1, a = 3),
    xlim = c(-4, 4)
  ) +
  
  labs(x = "Coefficient Value", y = "Penalty", color = "Penalty Function") +
  
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) + 
  
  reu_border


ggsave(
  filename = "lasso-scad-mcp-penalty.png",
  path = "./images",
  plot = penalty,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "lasso-scad-mcp-penalty.eps",
  path = "./images",
  plot = penalty,
  width = 10,
  height = 6,
  unit = "in"
)

# PLOT 2

# The following three functions compute the derivative of the 
# penalty for some coefficient value beta using the parameters lambda and a.

d.lasso <- function(beta, lambda)
{
  return(ifelse(beta < 0, -1, 1))
}

d.scad <- function(beta, lambda, a)
{
  beta <- abs(beta)
  output <- ifelse(beta < lambda, lambda, 0)
  output <- ifelse(beta >= lambda & beta < a * lambda,
                   (a * lambda - beta) / ((a - 1) * lambda),
                   output)
  output <- ifelse(beta >= a * lambda, 0, output)
  return(output)
}

d.mcp <- function(beta, lambda, a)
{
  beta <- abs(beta)
  return(ifelse(beta < a * lambda, sign(beta) * (lambda - beta / (a * lambda)), 0))
}

derivative <- ggplot() +
  geom_vline(
    data = data.frame(xint = c(1, 3)),
    mapping = aes(xintercept = xint),
    linetype = "dashed",
    color = "gray"
  ) +
  
  geom_function(
    mapping = aes(color = "LASSO"),
    fun = d.lasso,
    args = list(lambda = 1),
    xlim = c(0, 4)
  ) +
  
  geom_function(
    mapping = aes(color = "SCAD"),
    fun = d.scad,
    args = list(lambda = 1, a = 3),
    xlim = c(0, 4)
  ) +
  
  geom_function(
    mapping = aes(color = "MCP"),
    fun = d.mcp,
    args = list(lambda = 1, a = 3),
    xlim = c(0, 4)
  ) +
  
  labs(x = "Coefficient Value", y = "Penalty Derivative", color = "Penalty Function") +
  
  scale_x_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 1), limits = c(0, 1)) + 
  
  reu_border


ggsave(
  filename = "lasso-scad-mcp-derivative.png",
  path = "./images",
  plot = derivative,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "lasso-scad-mcp-derivative.eps",
  path = "./images",
  plot = derivative,
  width = 10,
  height = 6,
  unit = "in"
)

# PLOT 3

# The following three functions assume a model with one predictor and determine
# the predicted value for that coefficient based on the actual value of that
# coefficient. The predicted values are found for LASSO, SCAD, and MCP.

# Formulas for SCAD and MCP came from (Fan and Li, 2001) and (Zhang, 2010),
# respectively.

lasso.s <- function(x, lambda)
{
  return(ifelse(abs(x) < lambda, 0, sign(x) * (abs(x) - lambda)))
}

scad.s <- function(x, lambda, a)
{
  output <- ifelse(abs(x) < lambda, 0, 0)
  output <- ifelse(abs(x) >= lambda & abs(x) < 2 * lambda,
                   sign(x) * (abs(x) - lambda),
                   output)
  output <- ifelse(abs(x) >= 2 * lambda & abs(x) < a * lambda,
                   ((a - 1) * x - sign(x) * a * lambda) / (a - 2),
                   output)
  output <- ifelse(abs(x) >= a * lambda, x, output)
  return(output)
}

mcp.s <- function(x, lambda, a)
{
  output <- ifelse(abs(x) < lambda, 0, 0)
  output <- ifelse(abs(x) >= lambda & abs(x) < a * lambda,
                   sign(x) * (a * (abs(x) - lambda)) / (a - 1),
                   output)
  output <- ifelse(abs(x) >= a * lambda, x, output)
  return(output)
}

models <- data.frame(model_names = c("Lasso", "SCAD", "MCP", "OLS"))

solution <- ggplot(data = models) +
  geom_vline(
    data = data.frame(xint = c(-3, -1, 1, 3)),
    mapping = aes(xintercept = xint),
    linetype = "dashed",
    color = "gray"
  ) +
  
  geom_function(
    mapping = aes(color = "4OLS"),
    fun = function(x) {x},
    xlim = c(-4, 4),
    linetype = 2
  ) +
  
  geom_function(
    mapping = aes(color = "1Lasso"),
    fun = lasso.s,
    args = list(lambda = 1),
    xlim = c(-4, 4)
  ) +
  
  geom_function(
    mapping = aes(color = "2SCAD"),
    fun = scad.s,
    args = list(lambda = 1, a = 3),
    xlim = c(-4, 4)
  ) +
  
  geom_function(
    mapping = aes(color = "3MCP"),
    fun = mcp.s,
    args = list(lambda = 1, a = 3),
    xlim = c(-4, 4)
  ) +
  
  labs(x = expression(z), y = expression(hat(beta)), color = "Penalty Function") +
  
  scale_x_continuous(breaks = seq(-4, 4, by = 1), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 1), limits = c(-4, 4)) + 
  
  reu_border +
  
  scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF", "gray35"), labels = c("Lasso", "SCAD", "MCP", "OLS"))


ggsave(
  filename = "lasso-scad-mcp-solution.png",
  path = "./images",
  plot = solution,
  type = "cairo-png",
  width = 10,
  height = 6,
  unit = "in"
)

ggsave(
  filename = "lasso-scad-mcp-solution.eps",
  path = "./images",
  plot = solution,
  width = 10,
  height = 6,
  unit = "in"
)

