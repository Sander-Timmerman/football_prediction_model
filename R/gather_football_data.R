gather_football_data <- function(urls_fd) {
  flog.info("Starts gathering data from football_data")
  all_data <- data.frame()
  for (i in seq_len(nrow(urls_fd))) {
    df_football_data <- read_football_data_csv(as.character(urls_fd[i, 4]),
                                               as.character(urls_fd[i, 1]))
    if(nrow(df_football_data) > 0) {
      start_date <- determine_start_date(df_football_data)
      if(is.na(start_date)) {
        flog.error(paste0("Unknown date format in football_data csv for competition ",
                          urls_fd[i, 1],
                          ", season ",
                          urls_fd[i, 2],
                          ", level ",
                          urls_fd[i, 3]))
        next
      }
      temp_data <- prepare_football_data(df_football_data, urls_fd[i, 1], urls_fd[i, 2], start_date, urls_fd[i, 3])
      all_data <- rbind(all_data, temp_data)
    }
  }
  return(all_data)
}