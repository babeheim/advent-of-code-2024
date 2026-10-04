
rm(list = ls())

source("project_support.R")

tic.clearlog()

days <- list.files(".", pattern = "day")

tic("run 2023 advent")

for (i in seq_along(days)) {
  if (file.exists(file.path(days[i], "solution.R"))) {
    source(file.path(days[i], "solution.R"))
  }
}

toc(log = TRUE)

###########

tic.log(format = TRUE)
msg_log <- unlist(tic.log())

task <- msg_log
task <- gsub(":.*$", "", task)

time_min <- msg_log
time_min <- gsub("^.*: ", "", time_min)
time_min <- gsub(" sec elapsed", "", time_min)
time_min <- round(as.numeric(time_min)/60, 2)

report <- data.frame(
  machine = "thinkpad x360",
  task = task,
  time_min = time_min
)

write.csv(report, file.path("timing-report.csv"), row.names = FALSE)
