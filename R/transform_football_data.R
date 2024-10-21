transform_football_data <- function(df_football_data, competitie, seizoen, level, start_date) {
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