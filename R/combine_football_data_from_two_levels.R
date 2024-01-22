combine_football_data_from_two_levels <- function() {
  urls_fd_first_level <- read.csv("input/footballdata2.csv")
  urls_fd_second_level <- read.csv("input/footballdataniveautwee.csv")
  
  football_data_first_level <- gather_football_data(urls_fd_first_level, 1)
  football_data_second_level <- gather_football_data(urls_fd_second_level, 2)
  football_data <- rbind(football_data_first_level, football_data_second_level)
  return(football_data)
}