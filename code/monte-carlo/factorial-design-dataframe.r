library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))

# Used to generate factorial design
library(faux) # v1.0.0

n <- c(50, 200, 1000)
p <- c(10, 100, 2000)
st_dev <- c(1, 3, 6)
type <- c("independent", "symmetric", "autoregressive", "blockwise")
corr <- c(0.2, 0.5, 0.9)

parameters <- expand.grid(n, p, st_dev, type, corr)
colnames(parameters) <- c("n", "p", "st_dev", "type", "corr")

# Because corr is not used when type == "independent", we will remove all
# rows where type == "independent" and corr != 0.2. Then, the rows with
# type == "independent" will have their corr set to 0.
parameters <- parameters[!(parameters$type == "independent" & parameters$corr != 0.2), ]
parameters$corr <- ifelse(parameters$type == "independent",
                         0,
                         parameters$corr)

rownames(parameters) <- NULL

save(parameters, file = "../../data/monte-carlo/factorial-design.Rdata")