# R Script Testing for GNAR Models and observing prediction error
dir = paste(Sys.getenv("R_USER"), "/GitHub/GNAR-FYP", sep = "")
dir
setwd(dir)

library("GNAR")
library("igraph")
library("tidyverse")

