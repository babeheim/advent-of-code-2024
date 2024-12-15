
source("project_support.R")

count_xmas <- function(path, verbose = FALSE) {
  # path <- "day04/input.txt"
  raw <- readLines(path)

  mat <- matrix(NA, nrow = length(raw), ncol = nchar(raw[1]))
  for (i in 1:nrow(mat)) mat[i,] <- strsplit(raw[i], "")[[1]]

  stopifnot(all(mat %in% c("X", "M", "A", "S")))

  for (i in 1:prod(dim(mat))) {
    add <- data.frame(
      ind = i,
      row = (i - 1) %% nrow(mat) + 1,
      col = (i - 1) %/% nrow(mat) + 1,
      name = mat[i]
    )
    if (i == 1) {
      nodes <- add
    } else {
      nodes <- bind_rows(nodes, add)
    }
  }

  # note: ties are directional and so show up twice in this list
  # from, to, and direction are all stored
  # there's always an equivalent to, from with the opposite direction

  if (verbose) cat("counting north patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] > 1) {
      # add north edge
      alter_row <- nodes$row[i] - 1
      alter_col <- nodes$col[i]
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "north"
      )
      edges <- bind_rows(edges, add)
    }
  }

  nrow(edges)

  # north-only counts
  dir <- "north"
  n_north <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_north <- length(active_threads)
      }
    }
  }


  if (verbose) cat("counting south patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] < nrow(mat)) {
      # add south edge
      alter_row <- nodes$row[i] + 1
      alter_col <- nodes$col[i]
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "south"
      )
      edges <- bind_rows(edges, add)
    }
  }

  # south-only counts
  dir <- "south"
  n_south <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_south <- length(active_threads)
      }
    }
  }


  if (verbose) cat("counting west patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$col[i] > 1) {
      # add west edge
      alter_row <- nodes$row[i]
      alter_col <- nodes$col[i] - 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "west"
      )
      edges <- bind_rows(edges, add)
    }
  }


  # west-only counts
  dir <- "west"
  n_west <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_west <- length(active_threads)
      }
    }
  }


  if (verbose) cat("counting east patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$col[i] < ncol(mat)) {
      # add east edge
      alter_row <- nodes$row[i]
      alter_col <- nodes$col[i] + 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "east"
      )
      edges <- bind_rows(edges, add)
    }
  }

  # east-only counts
  dir <- "east"
  n_east <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_east <- length(active_threads)
      }
    }
  }


  if (verbose) cat("counting north-west patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] > 1 & nodes$col[i] > 1) {
      # add north-west edge
      alter_row <- nodes$row[i] - 1
      alter_col <- nodes$col[i] - 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "north-west"
      )
      edges <- bind_rows(edges, add)
    }
  }

  # north-west-only counts
  dir <- "north-west"
  n_north_west <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_north_west <- length(active_threads)
      }
    }
  }



  if (verbose) cat("counting north-east patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] > 1 & nodes$col[i] < ncol(mat)) {
      # add north-east edge
      alter_row <- nodes$row[i] - 1
      alter_col <- nodes$col[i] + 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "north-east"
      )
      edges <- bind_rows(edges, add)
    }
  }

  # north-east-only counts
  dir <- "north-east"
  n_north_east <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_north_east <- length(active_threads)
      }
    }
  }


  if (verbose) cat("counting south-west patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] < nrow(mat) & nodes$col[i] > 1) {
      # add south-west edge
      alter_row <- nodes$row[i] + 1
      alter_col <- nodes$col[i] - 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "south-west"
      )
      edges <- bind_rows(edges, add)
    }
  }

  # south-west-only counts
  dir <- "south-west"
  n_south_west <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_south_west <- length(active_threads)
      }
    }
  }


  if (verbose) cat("counting south-east patterns \n")

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] < nrow(mat) & nodes$col[i] < ncol(mat)) {
      # add south-east edge
      alter_row <- nodes$row[i] + 1
      alter_col <- nodes$col[i] + 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "south-east"
      )
      edges <- bind_rows(edges, add)
    }
  }

  # south-east-only counts
  dir <- "south-east"
  n_south_east <- 0
  active_threads <- which(edges$from_name == "X" & edges$to_name == "M" & edges$dir == dir)
  if (length(active_threads) > 0) {
    active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "A" & edges$dir == dir)
    if (length(active_threads) > 0) {
      active_threads <- which(edges$from_ind %in% edges$to_ind[active_threads] & edges$to_name == "S" & edges$dir == dir)
      if (length(active_threads) > 0) {
        n_south_east <- length(active_threads)
      }
    }
  }


  # now to compute stuff!

  out <- n_north +
  n_south +
  n_east +
  n_west +
  n_north_east +
  n_north_west +
  n_south_east +
  n_south_west

  return(out)

}


count_x_mas <- function(path, verbose = FALSE) {

  raw <- readLines(path)

  mat <- matrix(NA, nrow = length(raw), ncol = nchar(raw[1]))
  for (i in 1:nrow(mat)) mat[i,] <- strsplit(raw[i], "")[[1]]

  stopifnot(all(mat %in% c("X", "M", "A", "S")))

  nodes <- data.frame(ind = integer(), row = integer(), col = integer(), name = character())

  A_ind <- which(mat == "A")

  for (i in 1:length(A_ind)) {
    add <- data.frame(
      ind = A_ind[i],
      row = (A_ind[i] - 1) %% nrow(mat) + 1,
      col = (A_ind[i] - 1) %/% nrow(mat) + 1,
      name = mat[A_ind[i]]
    )
    nodes <- bind_rows(nodes, add)
  }

  # note: ties are directional and so show up twice in this list
  # from, to, and direction are all stored
  # there's always an equivalent to, from with the opposite direction

  edges <- data.frame(from_name = character(), from_ind = integer(), to_ind = integer(), to_name = character(), dir = character())

  if (verbose) cat("counting north-west patterns \n")

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] > 1 & nodes$col[i] > 1) {
      # add north-west edge
      alter_row <- nodes$row[i] - 1
      alter_col <- nodes$col[i] - 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "north-west"
      )
      if (alter_name %in% c("M", "S")) {
        edges <- bind_rows(edges, add)
      }
    }
  }

  if (verbose) cat("counting north-east patterns \n")

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] > 1 & nodes$col[i] < ncol(mat)) {
      # add north-east edge
      alter_row <- nodes$row[i] - 1
      alter_col <- nodes$col[i] + 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "north-east"
      )
      if (alter_name %in% c("M", "S")) {
        edges <- bind_rows(edges, add)
      }
    }
  }

  if (verbose) cat("counting south-west patterns \n")

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] < nrow(mat) & nodes$col[i] > 1) {
      # add south-west edge
      alter_row <- nodes$row[i] + 1
      alter_col <- nodes$col[i] - 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "south-west"
      )
      if (alter_name %in% c("M", "S")) {
        edges <- bind_rows(edges, add)
      }
    }
  }

  if (verbose) cat("counting south-east patterns \n")

  for (i in 1:nrow(nodes)) {
    if (nodes$row[i] < nrow(mat) & nodes$col[i] < ncol(mat)) {
      # add south-east edge
      alter_row <- nodes$row[i] + 1
      alter_col <- nodes$col[i] + 1
      alter_ind <- (alter_col - 1) * ncol(mat) + alter_row 
      alter_name <- mat[alter_ind]
      add <- data.frame(
        from_name = nodes$name[i],
        from_ind = nodes$ind[i],
        to_ind = alter_ind,
        to_name = alter_name,
        dir = "south-east"
      )
      if (alter_name %in% c("M", "S")) {
        edges <- bind_rows(edges, add)
      }
    }
  }

  nodes$n_S_alters <- NA
  nodes$n_M_alters <- NA
  for (i in 1:nrow(nodes)) {
    nodes$n_S_alters[i] <- sum(edges$to_name[edges$from_ind == nodes$ind[i]] == "S")
    nodes$n_M_alters[i] <- sum(edges$to_name[edges$from_ind == nodes$ind[i]] == "M")
  }
  nodes$correct_alters <- nodes$n_S_alters == 2 & nodes$n_M_alters == 2

  check <- which(nodes$correct_alters)
  valid <- rep(NA, length(check)) 

  for (i in 1:length(check)) {
    my_S_alter_locations <- edges[which(edges$to_name == "S" & edges$from_ind == nodes$ind[check[i]]),]$dir

    if (all(my_S_alter_locations %in% c("north-west", "south-east")) | all(my_S_alter_locations %in% c("north-east", "south-west"))) {
      valid[i] <- FALSE
    } else {
      valid[i] <- TRUE
    }
  }

  out <- sum(valid)
  return(out)

}


stopifnot(count_xmas("day04/test_input.txt") == 18)

tic("day 04, part 1")
stopifnot(count_xmas("day04/input.txt") == 2560)
toc(log = TRUE) # 150 seconds, not the best


stopifnot(count_x_mas("day04/test_input.txt") == 9)

tic("day 04, part 2")
stopifnot(count_x_mas("day04/input.txt") == 1910)
toc(log = TRUE)
