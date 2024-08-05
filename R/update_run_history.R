update_run_history <- function(run_number, success) {
  run_history <- read.csv("run_history.csv", stringsAsFactors = FALSE)
  run_history[run_number, "success"] <- if(success) "succeeded" else "failed"
  write.csv(run_history, file = "run_history.csv", row.names = FALSE)
  return(NULL)
}