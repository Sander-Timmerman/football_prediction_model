gather_football_data <- function(urls_fd, local_input) {
  flog.info("Starts gathering data from football_data")
  all_data <- data.frame(HomeTeam = character(),
                         AwayTeam = character(),
                         FTHG = integer(),
                         FTAG = integer(),
                         HS = integer(),
                         AS = integer(),
                         HST = integer(),
                         AST = integer(),
                         HPts = integer(),
                         APts = integer(),
                         Wedstrijdnummer = integer(),
                         Aantal = integer(),
                         Aantalwedstrijden = integer(),
                         Competitie = character(),
                         Seizoen = integer(),
                         Startdatum = integer(),
                         Niveau = integer())
  
  for (i in seq_len(nrow(urls_fd))) {
    df_football_data <- read_football_data_csv(as.character(urls_fd[i, 4]),
                                               as.character(urls_fd[i, 1]),
                                               as.character(urls_fd[i, 2])) %>%
      prepare_football_data(urls_fd[i, 1], urls_fd[i, 2], urls_fd[i, 3], local_input)
    
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
      temp_data <- transform_football_data(df_football_data, urls_fd[i, 1], urls_fd[i, 2], urls_fd[i, 3], start_date)
      all_data <- rbind(all_data, temp_data)
    }
  }
  
  flog.info("Finished gathering football data")
  return(all_data)
}