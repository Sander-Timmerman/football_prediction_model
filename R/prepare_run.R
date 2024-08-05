prepare_run <- function(param_json) {
  if(!file.exists("run_history.csv")) {
    write.csv(data.frame(run_number = integer(),
                         time = integer(),
                         success = character(),
                         parameters = character()), file = "run_history.csv", row.names = FALSE)
  }
  
  run_history <- read.csv("run_history.csv", stringsAsFactors = FALSE)
  run_history$time <- as.POSIXct(run_history$time)
  n_runs <- nrow(run_history)
  run_number <- if(n_runs > 0) n_runs + 1 else 1
  run_history <- rbind(run_history, data.frame(run_number = run_number,
                                               time = as.POSIXct(Sys.time()),
                                               success = "not completed",
                                               parameters = param_json))
  write.csv(run_history, file = "run_history.csv", row.names = FALSE)
  
  if(!dir.exists("cache")) {
    dir.create("cache")
  }
  dir.create(file.path("cache", run_number))
  
  if(!dir.exists("output")) {
    dir.create("output")
  }
  dir.create(file.path("output", run_number))
  
  return(run_number)
}