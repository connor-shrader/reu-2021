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
set.set(35246) # to generate the same data
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