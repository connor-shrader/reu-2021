# PENALTY.R

# This script file generates three of the plots shown in variable-selection.tex.

# This function returns -1 if x is negative and 1 if x is positive.
sign <- function(x)
{
  if (x < 0)
  {
    -1
  }
  else
  {
    1
  }
}

# Parameter values used to compute LASSO, SCAD, and MCP.
lambda <- 2
a <- 3

# Number of points for the plots
len <- 500

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
  if (beta < lambda)
  {
    lambda * beta
  }
  else if (beta < a * lambda)
  {
    (2 * a * lambda * beta - beta^2 - lambda^2) / (2 * (a - 1))
  }
  else
  {
    (lambda^2 * (a + 1)) / 2
  }
}

mcp <- function(beta, lambda, a)
{
  beta <- abs(beta)
  if (beta < a * lambda)
  {
    lambda * beta - (beta^2) / (2 * a)
  }
  else
  {
    1/2 * a * lambda^2
  }
}

# Generate the plot
x <- seq(-8, 8, length = len)

lasso.y <- lapply(x, lasso, lambda = 1)
scad.y <- lapply(x, scad, lambda = 1, a = 4)
mcp.y <- lapply(x, mcp, lambda = 1, a = 4)

plot(x, lasso.y,
     type = "l",
     xlab = expression(beta),
     ylab = "Penalty",
     col = "red",
     lwd = 3
)
lines(x, scad.y, col = "blue", lwd = 3)
lines(x, mcp.y, col = "green", lwd = 3)

legend(x = -8,
       y = 1.5,
       legend = c("LASSO", "SCAD", "MCP"),
       col = c("red", "blue", "green"),
       lty = c("solid", "solid", "solid")
)

# PLOT 2

# The following three functions compute the derivative of the 
# penalty for some coefficient value beta using the parameters lambda and a.

d.lasso <- function(beta, lambda)
{
  if (beta < 0)
  {
    -1
  }
  else
  {
    1
  }
}

d.scad <- function(beta, lambda, a)
{
  beta <- abs(beta)
  if (beta < lambda)
  {
    lambda
  }
  else if (beta < a * lambda)
  {
    (a * lambda - beta) / (a - 1)
  }
  else
  {
    0
  }
}

d.mcp <- function(beta, lambda, a)
{
  beta <- abs(beta)
  
  if (beta < a * lambda)
  {
    sign(beta) * (lambda - beta / a)
  }
  else
  {
    0
  }
}

# Generate the plot
x <- seq(0.0001, 8, length = len)

d.lasso.y <- lapply(x, d.lasso, lambda = 1)
d.scad.y <- lapply(x, d.scad, lambda = 1, a = 4)
d.mcp.y <- lapply(x, d.mcp, lambda = 1, a = 4)

plot(x, d.lasso.y,
     type = "l",
     xlab = expression(beta),
     ylab = "Derivative of Penalty",
     col = "red",
     lwd = 2,
     ylim = c(0, 1.3)
)
lines(x, d.scad.y, col = "blue", lwd = 2)
lines(x, d.mcp.y, col = "green", lwd = 2)

legend(x = 0,
       y = 1.3,
       legend = c("LASSO", "SCAD", "MCP"),
       col = c("red", "blue", "green"),
       lty = c("solid", "solid", "solid")
)

# PLOT 3

# The following three functions assume a model with one predictor and determine
# the predicted value for that coefficient based on the actual value of that
# coefficient. The predicted values are found for LASSO, SCAD, and MCP.

# Formulas for SCAD and MCP came from (Fan and Li, 2001) and (Zhang, 2010),
# respectively.

lasso.s <- function(x, lambda)
{
  if (abs(x) < lambda)
  {
    0
  }
  else
  {
    sign(x) * (abs(x) - lambda)
  }
}

scad.s <- function(x, lambda, a)
{
  if (abs(x) < lambda)
  {
    0
  }
  else if (abs(x) < 2 * lambda)
  {
    sign(x) * (abs(x) - lambda)
  }
  else if (abs(x) < a * lambda)
  {
    ((a - 1) * x - sign(x) * a * lambda) / (a - 2)
  }
  else
  {
    x
  }
}

mcp.s <- function(x, lambda, a)
{
  if (abs(x) < lambda)
  {
    0
  }
  else if (abs(x) < a * lambda)
  {
    sign(x) * (a * (abs(x) - lambda)) / (a - 1)
  }
  else
  {
    x
  }
}

# Generate the plot
x <- seq(-8, 8, length = len)

lasso.s.y <- lapply(x, lasso.s, lambda = lambda)
scad.s.y <- lapply(x, scad.s, lambda = lambda, a = a)
mcp.s.y <- lapply(x, mcp.s, lambda = lambda, a = a)

plot(x, x,
     type = "l",
     xlab = "Actual Coefficient",
     ylab = "Predicted Coefficient",
     col = "gray",
     lty = "dashed",
     lwd = 3,
)
lines(x, scad.s.y, col = "blue", lwd = 3)
lines(x, mcp.s.y, col = "green", lwd = 3)
lines(x, lasso.s.y, col = "red", lwd = 3)

legend(x = -8,
       y = 8,
       legend = c("LASSO", "SCAD", "MCP", "OLS"),
       col = c("red", "blue", "green", "gray"),
       lty = c("solid", "solid", "solid", "dashed")
)