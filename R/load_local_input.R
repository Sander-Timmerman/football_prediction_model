load_local_input <- function() {
  local_input <- list()
  local_input$data_source_info <- read.csv("input/data_source_info.csv", stringsAsFactors = FALSE)
  local_input$names <- read.csv("input/all_club_names.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  local_input$months <- read.csv("input/maanden.csv", colClasses = c(rep("character", 2)))
  return(local_input)
}