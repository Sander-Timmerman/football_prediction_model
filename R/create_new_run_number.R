create_new_run_number <- function(param_json) {
  run_history <- read.csv("run_history.csv", stringsAsFactors = FALSE)
  run_history$time <- as.POSIXct(run_history$time)
  n_runs <- nrow(run_history)
  run_number <- if(n_runs > 0) n_runs + 1 else 1
  run_history <- rbind(run_history, data.frame(run_number = run_number,
                                               time = as.POSIXct(Sys.time()),
                                               success = "not completed",
                                               parameters = param_json))
  write.csv(run_history, file = "run_history.csv", row.names = FALSE)
  return(run_number)
}