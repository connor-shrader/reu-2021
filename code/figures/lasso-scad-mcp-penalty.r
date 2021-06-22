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
  abs(beta) * lambda
}

scad <- function(beta, lambda, a)
{
  beta <- abs(beta)
  output <- ifelse (beta < lambda, lambda * beta, 0)
  output <- ifelse(beta >= lambda & beta < a * lambda,
                   (2 * a * lambda * beta - beta^2 - lambda^2) / (2 * (a - 1)),
                   output)
  output <- ifelse(beta >= a * lambda, (lambda^2 * (a + 1)) / 2, output)
  return(output)
}

mcp <- function(beta, lambda, a)
{
  beta <- abs(beta)
  output <- ifelse(beta < a * lambda, lambda * beta - (beta^2) / (2 * a), 0)
  output <- ifelse(beta >= a * lambda, 1/2 * a * lambda^2, output)
  return(output)
}

penalty <- ggplot() +
  geom_function(
    mapping = aes(color = "LASSO"),
    fun = lasso,
    args = list(lambda = 1),
    xlim = c(-6, 6)
  ) +
  
  geom_function(
    mapping = aes(color = "SCAD"),
    fun = scad,
    args = list(lambda = 1, a = 4),
    xlim = c(-6, 6)
  ) +
  
  geom_function(
    mapping = aes(color = "MCP"),
    fun = mcp,
    args = list(lambda = 1, a = 4),
    xlim = c(-6, 6)
  ) +
  
  geom_vline(
    data = data.frame(xint = c(-4, -1, 1, 4)),
    mapping = aes(xintercept = xint),
    linetype = "dashed",
    color = "gray"
  ) +
  
  xlab(label = "Coefficient Value") +
  ylab(label = "Penalty") +
  
  scale_x_continuous(breaks = seq(-6, 6, by = 1), limits = c(-6, 6)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) + 
  
  reu_border +
  theme(legend.title = element_blank(),
        text = element_text(size = 16))


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