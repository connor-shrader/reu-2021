# Contains the College dataset.
library(ISLR)

# Plots will be generated in a 1 * 2 grid.
par(mfrow = c(1, 2))

# The leaps library contains functions for best subset selection,
# forward stepwise selection and backward stepwise selection.
library(leaps)

# Fit a best subset selection model with up to 18 variables
bs <- regsubsets(Grad.Rate ~ .,
                  data = College,
                  nvmax = 18)

bs.summary <- summary(bs)

# Plot R^2 and BIC for the best model for every given number of variables.
# The number of variables that results in the lowest BIC is colored red.
plot(bs.summary$rsq, xlab = "Number of variables",
      ylab = "R^2", type = "l")
plot(bs.summary$bic, xlab = "Number of variables",
     ylab = "BIC", type = "l")
min.bic <- which.min(bs.summary$bic)
points(min.bic, bs.summary$bic[min.bic], col = "red",
       cex = 2, pch = 20)

# Repeat the same process for forward stepwise selection.
fss <- regsubsets(Grad.Rate ~ .,
                  data = College,
                  nvmax = 18,
                  method = "forward")
fss.summary <- summary(fss)

plot(fss.summary$rsq, xlab = "Number of variables",
     ylab = "R^2", type = "l")
plot(fss.summary$bic, xlab = "Number of variables",
     ylab = "BIC", type = "l")
min.bic <- which.min(fss.summary$bic)
points(min.bic, fss.summary$bic[min.bic], col = "red",
       cex = 2, pch = 20)

# Repeat the same process for backward stepwise selection.
bss <- regsubsets(Grad.Rate ~ .,
                  data = College,
                  nvmax = 18,
                  method = "backward")
bss.summary <- summary(fss)

plot(bss.summary$rsq, xlab = "Number of variables",
     ylab = "R^2", type = "l")
plot(bss.summary$bic, xlab = "Number of variables",
     ylab = "BIC", type = "l")
min.bic <- which.min(bss.summary$bic)
points(min.bic, bss.summary$bic[min.bic], col = "red",
       cex = 2, pch = 20)

# Plot the coefficients for the best models with 12 parameters.
# Notice that all three models are different.
coef(bs, 12)
coef(fss, 12)
coef(bss, 12)