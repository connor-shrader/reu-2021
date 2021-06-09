# Data generating

# x - nxp matrix of p independent predictors generated from N(0,1)
# b - (p+1)x1 vector of parameters
# y - linear function of xb


# step 1 - defining the data generating function

lin.dat <- function(n, p){
  b = c(1, 2, -2, 0, 0, 0.5, 3, rep(0, (p-6))) ## p-6 >= 0
  x = cbind(1, matrix(rnorm(n*p), nrow = n, ncol = p))
  y = x%*%b + rnorm(n)
  dat = data.frame(cbind(y, x))
  colnames(dat) = c("y", paste("x", 0:p, sep=""))
  return(dat)
}

# step 2 - implementing the DG function
set.seed(35246) # to generate the same data
ex.dat <- lin.dat(n=100, p = 10) # n> p, p>6

library(tidyverse)
View(ex.dat)


# Stepwise selection
library(MASS)  # We need this package for stepwise selection

# Full model for backward selection
fm <- lm(y ~ ., data = ex.dat[, -2])

# Null model for forward selection
nm <- lm(y ~ 1, data = ex.dat)

# AIC and BIC model selection for forward
af = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="forward", k=2, trace=F, steps=3000) #AIC
summary(af)  # the final model selected by AIC forward method
bf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="forward", k=log(nrow(ex.dat)), trace=F, steps=3000) #BIC  
summary(bf)

# AIC and BIC model selection for backward
ab = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="backward", k=2, trace=F, steps=3000) #AIC
bb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="backward", k=log(nrow(ex.dat)), trace=F, steps=3000) #BIC  

# AIC and BIC model selection for stepwise forward
asf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="both", k=2, trace=F, steps=3000) #AIC
bsf = stepAIC(nm, scope=list(lower=nm, upper=fm), direction="both", k=log(nrow(ex.dat)), trace=F, steps=3000) #BIC  

# AIC and BIC model selection for stepwise backward
asb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="both", k=2, trace=F, steps=3000) #AIC
bsb = stepAIC(fm, scope=list(lower=nm, upper=fm), direction="both", k=log(nrow(ex.dat)), trace=F, steps=3000) #BIC 

# Putting these models into a data frame

# Putting each model into a data frame manually.

df <- data.frame(row.names = paste("x", 0:10, sep = ""))
df$af <- unlist(lapply(names(fm$coefficients), function(str) af$coefficients[str]))
df$bf <- unlist(lapply(names(fm$coefficients), function(str) bf$coefficients[str]))
df$ab <- unlist(lapply(names(fm$coefficients), function(str) ab$coefficients[str]))
df$bb <- unlist(lapply(names(fm$coefficients), function(str) bb$coefficients[str]))
df$asf <- unlist(lapply(names(fm$coefficients), function(str) asf$coefficients[str]))
df$bsf <- unlist(lapply(names(fm$coefficients), function(str) bsf$coefficients[str]))
df$asb <- unlist(lapply(names(fm$coefficients), function(str) asb$coefficients[str]))
df$bsb <- unlist(lapply(names(fm$coefficients), function(str) bsb$coefficients[str]))

# Putting each mode into a data frame with a loop.

models <- list(af = af, bf = bf, ab = ab, bb = bb, asf = asf, bsf = bsf, asb = asb, bsb = bsb)
row.names <- c("(Intercept)", paste("x", 1:10, sep = ""))

get.coef <- function(model)
{
  unlist(lapply(row.names, function(str) model$coefficients[str]))
}

df <- data.frame(lapply(models, get.coef), row.names = row.names)
