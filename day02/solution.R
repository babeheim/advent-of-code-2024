
check_safety <- function(x, part1 = TRUE) {
  # if jump to next one is bad, see if jump to one after is good
  diffs <- diff(x)
  n_pos_diffs <- sum(diffs > 0)
  n_neg_diffs <- sum(diffs < 0)
  # ascending only if majority go up
  is_asc <- n_pos_diffs > n_neg_diffs
  diff_sign_good <- (diffs > 0 & is_asc) | (diffs < 0 & !is_asc)
  diff_size_good <- abs(diffs) >= 1 & abs(diffs) <= 3
  diff_is_bad <- !(diff_sign_good & diff_size_good)
  if (any(diff_is_bad)) {
    if (part1) {
      out <- FALSE
    } else {
      x_prime_safes <- rep(NA, length(x))
      for (i in 1:length(x)) {
        x_prime <- x[-i]
        x_prime_safes[i] <- check_safety(x_prime, part1 = TRUE)
      }
      if (any(x_prime_safes)) {
        out <- TRUE
      } else {
        out <- FALSE
      }
    }
  } else {
    out <- TRUE
  }
  return(out)
}

check_reports <- function(path, part1 = TRUE) {
  raw <- readLines(path)
  dat <- vector("list", length(raw))
  for (i in 1:length(raw)) dat[[i]] <- as.numeric(strsplit(raw[i], split = " ")[[1]])
  out <- sum(unlist(lapply(dat, check_safety, part1)))
  return(out)
}

stopifnot(check_reports("day02/test_input.txt") == 2)

tic("day 02, part 1")
stopifnot(check_reports("day02/input.txt") == 341)
toc(log = TRUE)

stopifnot(check_reports("day02/test_input.txt", part1 = FALSE) == 4)

tic("day 02, part 2")
stopifnot(check_reports("day02/input.txt", part1 = FALSE) == 404)
toc(log = TRUE)
