prepare_football_data <- function(df_football_data, competitie, seizoen, start_date, level, namen) {
  df_football_data <- df_football_data[nchar(df_football_data[[1]]) > 0, ]
  
  if(is.null(df_football_data$HS)) {
    df_football_data$HS <- NA
    df_football_data$AS <- NA
    df_football_data$HST <- NA
    df_football_data$AST <- NA
  }
  
  if(any((!df_football_data$HomeTeam %in% namen$Football_data |
          !df_football_data$AwayTeam %in% namen$Football_data) &
         level == 1)) {
    flog.warn(paste0("At least one team has an unknown name in football_data for competition ", 
                     competitie,
                     " and season ", 
                     seizoen,
                     ". This might cause problems when joining with Transfermarkt data"))
  }
  
  if(competitie == "Belgie" & seizoen == 25) {
    df_football_data$HS[69] <- 16
    df_football_data$AS[69] <- 4
    df_football_data$HST[69] <- 6
    df_football_data$AST[69] <- 1
  }
  
  temp_data_thuis <- df_football_data %>%
    select(HomeTeam, AwayTeam, FTHG, FTAG, FTR, HS, AS, HST, AST) %>%
    mutate(HPts = ifelse(FTR == "H", 3, ifelse(FTR == "A", 0, 1)),
           APts = ifelse(FTR == "A", 3, ifelse(FTR == "H", 0, 1)),
           Wedstrijdnummer = row_number()) %>%
    select(-FTR)
  away_order <- c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 11)
  temp_data_uit <- temp_data_thuis
  colnames(temp_data_uit) <- colnames(temp_data_uit)[away_order]
  
  temp_data <- rbind(temp_data_thuis, temp_data_uit) %>%
    arrange(Wedstrijdnummer) %>%
    group_by(HomeTeam) %>%
    mutate(Aantal = row_number(),
           Aantalwedstrijden = max(Aantal),
           Competitie = competitie,
           Seizoen = seizoen,
           Startdatum = start_date,
           Niveau = level) %>%
    ungroup()

  return(temp_data)
}
