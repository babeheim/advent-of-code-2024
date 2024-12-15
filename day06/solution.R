

sim_path <- function(map, part1 = TRUE) {

  barriers <- data.frame(
    ind = which(map == "#")
  )

  barriers$row <- (barriers$ind - 1) %% nrow(map) + 1
  barriers$col <- (barriers$ind - 1) %/% nrow(map) + 1

  pos <- data.frame(
    ind = which(map == "^")
  )
  pos$row <- (pos$ind - 1) %% nrow(map) + 1
  pos$col <- (pos$ind - 1) %/% nrow(map) + 1
  pos$direction <- "north"

  hit <- matrix(FALSE, ncol = ncol(map), nrow = nrow(map))

  hit_north <- matrix(FALSE, ncol = ncol(map), nrow = nrow(map))
  hit_south <- matrix(FALSE, ncol = ncol(map), nrow = nrow(map))
  hit_east <- matrix(FALSE, ncol = ncol(map), nrow = nrow(map))
  hit_west <- matrix(FALSE, ncol = ncol(map), nrow = nrow(map))

  looped <- FALSE

  while (!looped & !is.na(pos$direction)) {

    if (pos$direction == "north") {
      if (any(barriers$row < pos$row & barriers$col == pos$col)) {
        tar <- which(barriers$row < pos$row & barriers$col == pos$col)
        next_col <- pos$col
        next_row <- max(barriers$row[tar]) + 1
        next_direction <- "east"
      } else {
        next_col <- pos$col
        next_row <- 1
        next_direction <- NA
      }
    }

    if (pos$direction == "east") {
      if (any(barriers$row == pos$row & barriers$col > pos$col)) {
        tar <- which(barriers$row == pos$row & barriers$col > pos$col)
        next_col <- min(barriers$col[tar]) - 1
        next_row <- pos$row
        next_direction <- "south"
      } else {
        next_col <- ncol(map)
        next_row <- pos$row
        next_direction <- NA
      }
    }

    if (pos$direction == "south") {
      if (any(barriers$row > pos$row & barriers$col == pos$col)) {
        tar <- which(barriers$row > pos$row & barriers$col == pos$col)
        next_col <- pos$col
        next_row <- min(barriers$row[tar]) - 1
        next_direction <- "west"
      } else {
        next_col <- pos$col
        next_row <- nrow(map)
        next_direction <- NA
      }
    }

    if (pos$direction == "west") {
      if (any(barriers$row == pos$row & barriers$col < pos$col)) {
        tar <- which(barriers$row == pos$row & barriers$col < pos$col)
        next_col <- max(barriers$col[tar]) + 1
        next_row <- pos$row
        next_direction <- "north"
      } else {
        next_col <- 1
        next_row <- pos$row
        next_direction <- NA
      }
    }

    # track where agent goes
    if (pos$direction == "north") {
      if (any(hit_north[next_row:pos$row, next_col:pos$col])) looped <- TRUE
      hit_north[next_row:pos$row, next_col:pos$col] <- TRUE
    } else if (pos$direction == "south") {
      if (any(hit_south[next_row:pos$row, next_col:pos$col])) looped <- TRUE
      hit_south[next_row:pos$row, next_col:pos$col] <- TRUE
    } else if (pos$direction == "east") {
      if (any(hit_east[next_row:pos$row, next_col:pos$col])) looped <- TRUE
      hit_east[next_row:pos$row, next_col:pos$col] <- TRUE
    } else if (pos$direction == "west") {
      if (any(hit_west[next_row:pos$row, next_col:pos$col])) looped <- TRUE
      hit_west[next_row:pos$row, next_col:pos$col] <- TRUE
    }
    hit[next_row:pos$row, next_col:pos$col] <- TRUE
    # update agent position
    pos$row <- next_row
    pos$col <- next_col
    pos$direction <- next_direction

  }

  if (part1) {
    return(hit)
  } else {
    return(looped)
  }
}

explore_paths <- function(path, part1 = TRUE) {
  raw <- readLines(path)

  map <- matrix(NA, ncol = nchar(raw[1]), nrow = length(raw))
  for (i in 1:nrow(map)) map[i,] <- strsplit(raw[i], split = "")[[1]]
 
  hits <- sim_path(map)

  if (part1) {
    out <- sum(hits)
  } else {
    check <- which(hits & map != "^")
    will_loop <- rep(NA, length(check))
    for (i in 1:length(check)) {
      map2 <- map
      map2[check[i]] <- "#"
      will_loop[i] <- sim_path(map2, part1 = FALSE)
    }
    out <- sum(will_loop)
  }

  return(out)

}

stopifnot(explore_paths("day06/test_input.txt") == 41)

tic("day 06, part 1")
stopifnot(explore_paths("day06/input.txt") == 4977)
toc(log = TRUE)

stopifnot(explore_paths("day06/test_input.txt", part1 = FALSE) == 6)

tic("day 06, part 2")
stopifnot(explore_paths("day06/input.txt", part1 = FALSE) == 1729)
toc(log = TRUE)

