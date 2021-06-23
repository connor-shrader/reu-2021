library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))

# Used to generate factorial design
library(faux) # v1.0.0

n <- c(50, 200, 1000)
p <- c(10, 100, 2000)
sigma <- c(1, 3, 6)
covar <- c("independent", "symmetric", "autoregressive", "blockwise")
rho <- c(0.2, 0.5, 0.9)

parameters <- expand.grid(n, p, sigma, covar, rho)
colnames(parameters) <- c("n", "p", "sigma", "covar", "rho")

# Because rho is not used when covar == "independent", we will remove all
# rows where covar == "independent" and rho != 0.2. Then, the rows with
# covar == "independent" will have their rho set to 0.
parameters <- parameters[!(parameters$covar == "independent" & parameters$rho != 0.2), ]
parameters$rho <- ifelse(parameters$covar == "independent",
                         0,
                         parameters$rho)

rownames(parameters) <- NULL

save(parameters, file = "../../data/monte-carlo/factorial-design.Rdata")