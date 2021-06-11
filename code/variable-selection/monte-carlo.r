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


# Lasso model for variable selection
library(glmnet)
lasso <- cv.glmnet(x = as.matrix(ex.dat[,-1]), y = ex.dat$y, alpha = 1)


# Ridge model for dealing with multicollinearity
ridge <- cv.glmnet(x = as.matrix(ex.dat[,-1]), y = ex.dat$y, alpha = 0)


# Elastic Net model for multicollinearity and variable selection
enet <- cv.glmnet(x = as.matrix(ex.dat[,-1]), y = ex.dat$y, alpha = 0.8) #small alpha is not needed since small multicollinearity

# MCP
library(ncvreg)

scad <- cv.ncvreg(X = ex.dat[, -1], y = ex.dat$y, penalty = "SCAD")
#scad_c <- coef(scad, lambda = scad$lambda.min)

mcp <- cv.ncvreg(X = ex.dat[, -1], y = ex.dat$y)
#mcp_c <- coef(mcp, lambda = mcp$lambda.min)
# calling coef(mcp, lambda = 0.05) has two intercepts?


#####Putting Models into DataFrame#####


# Putting these models into a data frame

# Connor's method

# List of all of the models. I did not include lasso, ridge, and enet because I
# could not get them to work with my method.
models <- list(fm = fm, nm = nm, af = af, bf = bf, ab = ab, bb = bb, asf = asf, bsf = bsf,
               asb = asb, bsb = bsb, scad = scad, mcp = mcp)

# Names of the rows for the final dataframe: (Intercept), x1, x2, ..., xp
row.names <- c("(Intercept)", paste("x", 1:10, sep = ""))

# get.coef inputs a model and returns a vector with the values for each coefficient.
# Depending on the model, unused variables will be set to 0 or NA. A later line will
# make the outputs consistent.
get.coef <- function(model)
{
  coef_values <- unlist(lapply(row.names, function(predictor) coef(model)[predictor]))
}

# Create a new dataframe. The rows are the predictors, and the columns are the
# models. The (i, j) entry contains the coefficient for predictor i using model j.
df <- data.frame(lapply(models, get.coef), row.names = row.names)

# Sets all zero coefficients to NA (this makes it easier to read).
df[df == 0] <- NA



# Gabe's probably slower method to put models into a dataframe
library(dplyr)
multi.merge <-function(model_list, col_names){ #takes input of list of lm models, and vector of column names
  for (i in 1:length(model_list)){
    model_list[[i]] <- data.frame(as.matrix(coef(model_list[[i]]))) #turns lm model class into dataframe of coefficients
    model_list[[i]]$betas <- row.names(model_list[[i]]) #adds column of beta coefficient names
  }
  
  # ugly code to rearrange order of beta column and full model column
  full_df <- model_list[[1]]
  full_df <- full_df[-1]
  full_df$fm <- model_list[[1]][[1]]
  
  for (i in 2:length(model_list)){
    full_df <- left_join(full_df, model_list[[i]], by = "betas") #joins code together by beta coefficient name
  }
  
  full_df[is.na(full_df)] <- 0
  colnames(full_df) <- c("betas", col_names) #renames columns according to what was input to the function
  
  return(full_df)
}

coefs_df <- multi.merge(list(fm, af, bf, ab, bb, asf, bsf, asb, bsb, mcp, scad, lasso, ridge, enet), 
                        c("fm", "af", "bf", "ab", "bb", "asf", "bsf", "asb", "bsb", "mcp", "scad", "lasso", "ridge", "elastic_net"))



#####Monte Carlo Replication Function #####
calc_mse <- function(model, test_dat) {
  if (class(model) == "cv.ncvreg") { #checks for mcp or scad model
    y_hat <-  data.frame(predict(model, X = as.matrix(test_dat[,-1])))
  }
  else if (class(model) == "cv.glmnet") { #check for lasso, ridge, enet model
    y_hat <-  data.frame(predict(model, newx = as.matrix(test_dat[,-1])))
  }
  else { #rest is lm models
    y_hat <- data.frame(predict(model, newdata = test_dat[,-1]))
  }
  
  y <- test_dat[,1]
  mse <- mean(((y - y_hat)^2)[,1]) #take mean of residuals squared
  return(mse)
}

monte_carlo <- function(seed){
  set.seed(seed) # to generate the same data
  all.dat <- lin.dat(n=200, p = 10) # n> p, p>6
  ex.dat <- all.dat[1:100,]
  test.dat <- all.dat[101:nrow(all.dat), ]
  
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
  
  # Lasso model for variable selection
  lasso <- cv.glmnet(x = as.matrix(ex.dat[,-1]), y = ex.dat$y, alpha = 1)
  
  # Ridge model for dealing with multicollinearity
  ridge <- cv.glmnet(x = as.matrix(ex.dat[,-1]), y = ex.dat$y, alpha = 0)
  
  # Elastic Net model for multicollinearity and variable selection
  enet <- cv.glmnet(x = as.matrix(ex.dat[,-1]), y = ex.dat$y, alpha = 0.8) #small alpha is not needed since small multicollinearity
  
  # MCP
  scad <- cv.ncvreg(X = ex.dat[, -1], y = ex.dat$y, penalty = "SCAD")
  #scad_c <- coef(scad, lambda = scad$lambda.min)
  
  mcp <- cv.ncvreg(X = ex.dat[, -1], y = ex.dat$y)
  #mcp_c <- coef(mcp, lambda = mcp$lambda.min)
  # calling coef(mcp, lambda = 0.05) has two intercepts?
  
  mse_list <- list(af = calc_mse(af, test.dat),  #creates list of mse for each model
                   bf = calc_mse(bf, test.dat),
                   ab = calc_mse(ab, test.dat),
                   bb = calc_mse(bb, test.dat),
                   asf = calc_mse(asf, test.dat),
                   bsf = calc_mse(bsf, test.dat),
                   asb = calc_mse(asb, test.dat),
                   bsb = calc_mse(bsb, test.dat),
                   mcp = calc_mse(mcp, test.dat),
                   scad = calc_mse(scad, test.dat),
                   lasso = calc_mse(lasso, test.dat),
                   ridge = calc_mse(ridge, test.dat),
                   enet = calc_mse(enet, test.dat))
  
  coefs_df <- multi.merge(list(fm, af, bf, ab, bb, asf, bsf, asb, bsb, mcp, scad, lasso, ridge, enet), 
                          c("fm", "af", "bf", "ab", "bb", "asf", "bsf", "asb", "bsb", "mcp", "scad", "lasso", "ridge", "elastic_net"))
  
  return(list(coefs_df, mse_list))
}

seeds <- list(100:110)

results <- lapply(seeds[[1]], monte_carlo)

