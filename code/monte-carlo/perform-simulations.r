library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("simulation.r")
source("metrics.r")

# Load parameters from file
load("../../data/monte-carlo/factorial-design.Rdata")

run_simulations <- function(indices) {
  iterations <- 1
  
  for (i in indices) {
    row <- parameters[i, ]
    print(row)
    n <- row$n
    p <- row$p
    st_dev <- row$sigma
    
    # Convert row$covar (which is a factor) to a character.
    type <- as.character(row$covar)
    corr <- row$rho
    
    filename <- paste("../../results/monte-carlo/sim_results_",
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
      
      print(type)
      print(block_size)
      results <- monte_carlo(n = n,
                             p = p,
                             iterations = iterations,
                             st_dev = st_dev,
                             type = type,
                             corr = corr,
                             block_size = block_size)
      
      saveRDS(results, file = filename)
    }
    else {
      warning(c("Results file already exists for row ", i, "."))
    }
  }
}

system.time(run_simulations(28))
