

mightCrash <- function() {
  x <- sample(1:2, 1)
  if (x == 1) {
    stop("Heads")
  }
  return("Tails")
}

finished_iter <- FALSE
while(!finished_iter) {
  tryCatch(
    {
      result <- mightCrash()
      finished_iter <- TRUE
      message(result)
    },
    error = function(error_message) {
      message(error_message)
    }
  )
}