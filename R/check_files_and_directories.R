check_files_and_directories <- function() {
  if(!file.exists("run_history.csv")) {
    write.csv(data.frame(run_number = integer(),
                         time = integer(),
                         success = character(),
                         parameters = character()), file = "run_history.csv", row.names = FALSE)
  }
  if(!dir.exists("cache")) {
    dir.create("cache")
  }
  if(!dir.exists("output")) {
    dir.create("output")
  }
  
  return(NULL)
}