rm(list = ls())
library(parallel)

generate_data <- function(seed, n, p, std = 1) {
  set.seed(seed)
  dat <- rnorm(n * p, sd = sd)
  mat <- matrix(dat, nrow = n, ncol = p)
  return(mat)
}

simulate_stuff <- function(n, p, iterations, ...) {
  ncores <- detectCores()
  cl <- makeCluster(floor(ncores / 2))
  tryCatch({
    clusterExport(cl, list("generate_data", "n", "p", "iterations"), envir = environment())
    #clusterExport(cl, list(...), envir = environment())
    res <- parLapply(cl,
                     1:iterations,
                     generate_data,
                     n = n,
                     p = p,
                     ...)
  },
  finally = {
    stopCluster(cl)
  })
  
  return(res)
}

system.time(results <- simulate_stuff(n = 10, p = 5, iterations = 1))


