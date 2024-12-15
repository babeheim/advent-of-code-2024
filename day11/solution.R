
source("project_support.R")

count_rocks <- function(path, n_blinks = 25, verbose = FALSE) {

  raw <- readLines(path)
  raw_vec <- as.numeric(strsplit(raw, split = " ")[[1]])
  states <- data.frame(
    name = raw_vec,
    count = NA
  )

  states$even <- nchar(states$name) %% 2 == 0

  states$blinks_completed <- 0

  for (i in 1:nrow(states)) states$count[i] <- sum(raw_vec == states$name[i])

  for (b in 1:n_blinks) {

    states_prime <- states
    states_prime$count <- 0

    for (i in 1:nrow(states)) {
      if (states$name[i] == 0) {
        new_state <- 1
        to_ind <- which(states_prime$name == new_state)
        states_prime$count[to_ind] <- states$count[i] + states_prime$count[to_ind]
      } else if (!states$even[i]) {
        new_state <- states$name[i] * 2024
        if (!(new_state %in% states_prime$name)) {
          add <- data.frame(name = new_state, count = 0, even = nchar(new_state) %% 2 == 0)
          states_prime <- bind_rows(states_prime, add)
        }
        to_ind <- which(states_prime$name == new_state)
        states_prime$count[which(states_prime$name == new_state)] <- states$count[i] + states_prime$count[which(states_prime$name == new_state)]
      } else {
        new_state <- as.numeric(substr(states$name[i], 1, nchar(states$name[i])/2))
        if (!(new_state %in% states_prime$name)) {
          add <- data.frame(name = new_state, count = 0, even = nchar(new_state) %% 2 == 0)
          states_prime <- bind_rows(states_prime, add)
        }
        to_ind <- which(states_prime$name == new_state)
        states_prime$count[to_ind] <- states$count[i] + states_prime$count[to_ind]

        new_state <- as.numeric(substr(states$name[i], nchar(states$name[i])/2 + 1, nchar(states$name[i])))
        if (!(new_state %in% states_prime$name)) {
          add <- data.frame(name = new_state, count = 0, even = nchar(new_state) %% 2 == 0)
          states_prime <- bind_rows(states_prime, add)
        }
        to_ind <- which(states_prime$name == new_state)
        states_prime$count[to_ind] <- states$count[i] + states_prime$count[to_ind]
      }
    }

    states <- states_prime

    states$blinks_completed <- b

    if (verbose) cat(b, "blinks completed\n")

  }

  out <- sum(states$count)
  return(out)

}

tic("day 11, part 1")
stopifnot(count_rocks("day11/input.txt") == 182081)
toc(log = TRUE)

tic("day 11, part 2")
stopifnot(count_rocks("day11/input.txt", n_blinks = 75) == 216318908621637)
toc(log = TRUE)
