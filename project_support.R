
library(tictoc)
library(dplyr)
library(igraph)
library(viridis)
library(parallel)
library(gtools)
library(stringr) # str_count
library(numbers) # for LCM function

options(scipen = 999)
options(warnPartialMatchDollar = TRUE)

str_to_mat <- function(x) {
  out <- matrix(NA, nrow = length(x), ncol = nchar(x[1]))
  for (i in 1:length(x)) {
    out[i,] <- strsplit(x[i], "")[[1]]
  }
  return(out)
} # generally useful for advent!

lcmer <- function(x) {
  out <- LCM(x[1], x[2])
  if (length(x) > 2) {
    for (i in 3:length(x)) {
      out <- LCM(out, x[i])
    }
  }
  return(out)
}
