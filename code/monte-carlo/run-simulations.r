# perform-simulations.r
# Gabe Ackall, Connor Shrader

# This file contains run_simulations(), a function that we used to perform
# Monte Carlo simulations using the functions contained in metrics.r
# and simulation.r.

rm(list = ls())

# Used to set current working directory to the location of this file.
library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("simulation-functions.r")
source("metrics.r")

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
run_simulations <- function(indices, response, iterations = 1, ...) {
  # print(list(...))
  all_results <- lapply(indices, function(i) {
    message("Beginning to run row ", i, ".")
    row <- parameters[i, ]
    n <- row$n
    p <- row$p
    st_dev <- row$st_dev
    
    # Convert row$type (which is a factor) to a character.
    type <- as.character(row$type)
    corr <- row$corr
    
    if (response == 1) {
      folder <- "monte-carlo-linear"
    }
    else if (response == 2) {
      folder <- "monte-carlo-nonlinear"
    }
    else {
      stop("Invalid response given: Use 1 for a linear relationship or 2 for 
            a non-linear relationship")
    }
    
    filename <- paste("../../results/", folder, "/sim_results_",
                      response, "_",
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
                                                       response = response,
                                                       iterations = iterations,
                                                       st_dev = st_dev,
                                                       type = type,
                                                       corr = corr,
                                                       block_size = block_size,
                                                       ...))
      
      message("Finished running row ", i, " at ", Sys.time(), ". Time taken: ")
      print(time_taken)
      
      saveRDS(results, file = filename)
      return(results)
    }
    else {
      message("Results file already exists for row ", i, ".")
    }
  })
  
  return(all_results)
}

# Run a line like the following to run simulations:
#
# res <- run_simulations(indices = 1, response = 1, iterations = 100)
