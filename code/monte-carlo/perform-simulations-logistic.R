# perform-simulations.r
# Gabe Ackall, Connor Shrader

# This file contains run_simulations(), a function that we used to perform
# Monte Carlo simulations using the functions contained in metrics-logistic.r
# and logistic-monte-carlo.r.

rm(list = ls())

# Used to set current working directory to the location of this file.
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("logistic-monte-carlo.r")
source("metrics-logistic.r")

# Load table of parameters from file.
parameters <- readRDS("../../data/monte-carlo/factorial-design.rds")

# This function takes in a vector of indices and the number of iterations.
# Then, this function iterates over the rows of the parameter table corresponding
# to each index and runs Monte Carlo simulations. The number of times each
# parameter combination is ran is determined by the iterations parameter.
# Then, the results for all of the simulations are saved to as an RDS file
# in reu-2021/results/monte-carlo.
#
# For example, calling run_simulations(1:5, 5) will run simulations using the
# parameter combinations from the first five rows of the parameter table
# contained in reu-2021/data/monte-carlo/factorial-design.Rdata. Each combination
# is ran 5 times.
#
# Note that there are 270 parameter combinations, so indices should be a subset
# of 1:270.
run_simulations <- function(indices, iterations = 1, ...) {
  results <- lapply(indices, function(i) {
    message("Beginning to run row ", i, ".")
    row <- parameters[i, ]
    n <- row$n
    p <- row$p
    st_dev <- row$st_dev
    
    # Convert row$type (which is a factor) to a character.
    type <- as.character(row$type)
    corr <- row$corr
    
    filename <- paste("../../results/monte-carlo/logistic/sim_results_",
                      n, "_", 
                      p, "_", 
                      st_dev, "_", 
                      type, "_", 
                      corr, ".rds", sep = "")
    
    block_size <- 0
    if (!file.exists(filename)) {
      if (type == "blockwise") {
        if (p == 10) {
          block_size <- 5
        }
        else if (p == 100) {
          block_size <- 25
        }
        else if (p == 2000) {
          block_size <- 100
        }
      }
      
      time_taken <- system.time(results <- monte_carlo(n = n,
                                                       p = p,
                                                       iterations = iterations,
                                                       st_dev = st_dev,
                                                       type = type,
                                                       corr = corr,
                                                       block_size = block_size,
                                                       ...))
      
      message("Finished running row ", i, " at ", Sys.time(), ". Time taken: ")
      print(time_taken)
      
      #saveRDS(results, file = filename)
    }
    else {
      message("Results file already exists for row ", i, ".")
    }
  })
  
  return(results)
}

# res <- run_simulations(indices = 99, iterations = 100)