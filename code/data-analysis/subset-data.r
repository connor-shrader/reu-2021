# test-plots.r
# Gabe Ackall, Connor Shrader

# This function takes in a data frame and a list of keyword arguments (...).
# This function then returns a subset of this dataframe that only contains rows
# where all the keyword arguments are satisfied. For example, if one calls
# subset_data(df, x = 3), then this function returns the rows of df such that
# df$x == 3.

# This function is used in ../facet-plots.r and ./generate-table.r.

subset_data <- function(data, ...) {
  args <- list(...)
  
  dat <- data
  for (fixed_parameter in names(args)) {
    dat <- dat[dat[[fixed_parameter]] == args[[fixed_parameter]], ]
  }
  
  return(dat)
}