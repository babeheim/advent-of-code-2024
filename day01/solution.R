
source("project_support.R")

calc_dist <- function(path, part1 = TRUE) {
  raw <- read.table(path, header = FALSE, sep = " ")
  d <- dplyr::select(raw, V1, V2 = V4)
  if (part1) {
    d$V1 <- sort(d$V1)
    d$V2 <- sort(d$V2)
    out <- sum(abs(d$V1 - d$V2))
  } else {
    d$n_times <- NA
    for (i in 1:nrow(d)) {
      d$n_times[i] <- sum(d$V2 %in% d$V1[i])
    }
    out <- sum(d$V1 * d$n_times)
  }
  return(out)
}

stopifnot(calc_dist("day01/test_input.txt") == 11)

tic("day 01, part 1")
stopifnot(calc_dist("day01/input.txt") == 2166959)
toc(log = TRUE)

stopifnot(calc_dist("day01/test_input.txt", part1 = FALSE) == 31)

tic("day 01, part 2")
stopifnot(calc_dist("day01/input.txt", part1 = FALSE) == 23741109)
toc(log = TRUE)
