
order_elements <- function(element_names, rules, verbose = FALSE) {

  # just focus on rules involving these named elements
  keep <- which(rules$from %in% element_names & rules$to %in% element_names)
  rules <- rules[keep,]

  elements <- data.frame(
    name = element_names,
    marked = FALSE,
    order = NA,
    ind = NA
  )

  order_counter <- 1
  
  for (i in 1:nrow(elements)) elements$ind[i] <- sum(rules$to == elements$name[i])

  while (!all(elements$marked)) {
    add_row <- which(elements$ind == 0 & !elements$marked)[1]
    if (is.na(add_row)) stop("no free elements!")
    elements$order[add_row] <- order_counter
    elements$marked[add_row] <- TRUE
    drop <- which(rules$from == elements$name[add_row])
    rules <- rules[-drop,]
    if (verbose) cat(length(drop), "rules dropped!\n")
    elements$ind <- NA
    for (i in 1:nrow(elements)) elements$ind[i] <- sum(rules$to == elements$name[i])
    order_counter <- order_counter + 1
  }

  elements <- arrange(elements, order)

  return(elements$name)

}

check_order <- function(path, part1 = TRUE) {
  raw <- readLines(path)
  my_sep <- which(raw == "")

  updates <- strsplit(raw[(my_sep+1):length(raw)], split = ",")
  updates <- lapply(updates, as.numeric)

  rules <- data.frame(
    string = raw[1:(my_sep-1)]
  )
  rules$from <- as.numeric(gsub("\\|.+$", "", rules$string))
  rules$to <- as.numeric(gsub("^\\d+\\|", "", rules$string))

  update_dat <- data.frame(
    length = unlist(lapply(updates, length)),
    pass = NA,
    middle = NA
  )

  reordered <- vector("list", length = length(updates))
  for (i in 1:nrow(update_dat)) {
    reordered[[i]] <- order_elements(updates[[i]], rules)
    update_dat$pass[i] <- all(reordered[[i]] == updates[[i]])
    update_dat$middle[i] <- reordered[[i]][ceiling(length(reordered[[i]])/2)]
  }

  if (part1) {
    out <- sum(update_dat$middle[which(update_dat$pass)])
  } else { 
    out <- sum(update_dat$middle[which(!update_dat$pass)])
  }
  return(out)

}

stopifnot(check_order("day05/test_input.txt") == 143)

tic("day 05, part 1")
stopifnot(check_order("day05/input.txt") == 5955)
toc(log = TRUE)

stopifnot(check_order("day05/test_input.txt", part1 = FALSE) == 123)

tic("day 05, part 2")
stopifnot(check_order("day05/input.txt", part1 = FALSE) == 4030)
toc(log = TRUE)

