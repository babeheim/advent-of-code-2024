
extract_pattern <- function(pattern, text, perl = TRUE) {
  all_matches <- gregexpr(pattern, text, perl = perl)
  matched_strings <- regmatches(text, all_matches)
  return(matched_strings[[1]])
}

calc_prods <- function(path, part1 = TRUE) {

  raw <- readLines(path)
  if (length(raw) > 1) raw <- paste(raw, collapse = "")
  stopifnot(length(raw) == 1)

  pattern <- "mul\\(\\d+,\\d+\\)"
  mult_matches <- gregexpr(pattern, raw, perl = TRUE)
  active_mults <- regmatches(raw, mult_matches)[[1]]

  if (!part1) {
    pattern <- "do\\(\\)"
    do_matches <- gregexpr(pattern, raw, perl = TRUE)
    pattern <- "don't\\(\\)"
    dont_matches <- gregexpr(pattern, raw, perl = TRUE)

    # strategy: go through each event one by one, and toggle the status of the multiplications as do or dont
    add <- data.frame(
      start_col = mult_matches[[1]],
      type = "mult",
      do = NA
    )
    events <- add

    add <- data.frame(
      start_col = do_matches[[1]],
      type = "do",
      do = NA
    )
    events <- bind_rows(events, add)
    events <- arrange(events, start_col)    

    add <- data.frame(
      start_col = dont_matches[[1]],
      type = "dont",
      do = NA
    )
    events <- bind_rows(events, add)
    events <- arrange(events, start_col)    

    do_state <- TRUE
    for (i in 1:nrow(events)) {
      if (events$type[i] == "dont") {
        do_state <- FALSE
      } else if (events$type[i] == "do") {
        do_state <- TRUE
      } else {
        events$do[i] <- do_state
      }
    }

    stopifnot(!any(events$type == "mult" & is.na(events$do)))

    events <- filter(events, type == "mult")
    keep <- which(events$do)
    active_mults <- active_mults[keep]
    
  }

  prods <- rep(NA, length(active_mults))
  for (i in 1:length(prods)) {
    digits <- extract_pattern("(?<=mul\\()\\d+,\\d+", active_mults[i])
    prods[i] <- prod(as.numeric(strsplit(digits, ",")[[1]]))
  }
  out <- sum(prods)
  return(out)
}

stopifnot(calc_prods("day03/test_input.txt") == 161)

tic("day 03, part 1")
stopifnot(calc_prods("day03/input.txt") == 183669043)
toc(log = TRUE)

stopifnot(calc_prods("day03/test_input2.txt", part1 = FALSE) == 48)

tic("day 03, part 2")
stopifnot(calc_prods("day03/input.txt", part1 = FALSE) == 59097164)
toc(log = TRUE)