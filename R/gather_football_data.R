gather_football_data <- function(urls_fd, level) {
  all_data <- data.frame()
  for (i in seq_len(nrow(urls_fd))) {
    df_football_data <- read.csv(as.character(urls_fd[i,3]))
    start_date <- determine_start_date(df_football_data)  
    temp_data <- prepare_football_data(df_football_data, urls_fd[i, 1], urls_fd[i, 2], start_date, level)
    all_data <- rbind(all_data, temp_data)
  }
  return(all_data)
}