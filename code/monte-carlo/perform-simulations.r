library(rstudioapi) # v0.13

setwd(dirname(getActiveDocumentContext()$path))
source("simulation.r")
source("metrics.r")

# Load parameters from file
load("../../data/monte-carlo/factorial-design.Rdata")
