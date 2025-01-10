prepare_football_data <- function(df_football_data, competitie, seizoen, level, local_input) {
  df_football_data <- df_football_data[nchar(df_football_data[[1]]) > 0, ]
  
  if(is.null(df_football_data$HS)) {
    df_football_data$HS <- NA
    df_football_data$AS <- NA
    df_football_data$HST <- NA
    df_football_data$AST <- NA
  }
  
  teams_to_ignore <- local_input$teams_to_ignore %>%
    filter(Competitie == competitie & Seizoen == seizoen & Niveau == level)
  
  df_football_data <- df_football_data %>%
    add_additional_football_data(local_input$additional_football_data, competitie, seizoen, level) %>%
    convert_football_data_date(competitie, seizoen, level) %>%
    distinct(HomeTeam, AwayTeam, .keep_all = TRUE) %>%
    filter(!HomeTeam %in% teams_to_ignore$Team & !AwayTeam %in% teams_to_ignore$Team)
  
  if(any((!df_football_data$HomeTeam %in% local_input$names$Football_data |
          !df_football_data$AwayTeam %in% local_input$names$Football_data) &
         level == 1)) {
    flog.warn(paste0("At least one team has an unknown name in football_data for competition ", 
                     competitie,
                     ", season ", 
                     seizoen,
                     " and level ",
                     level,
                     ". This might cause problems when joining with Transfermarkt data"))
  }
  
  if(!(sum(is.na(df_football_data$HS), 
           is.na(df_football_data$AS), 
           is.na(df_football_data$HST), 
           is.na(df_football_data$AST)) %in% c(0, nrow(df_football_data) * 4))) {
    flog.warn(paste0("The shot data is incomplete for some but not all matches for competition ", 
                     competitie,
                     ", season ", 
                     seizoen,
                     " and level ",
                     level,
                     ". This might cause NAs when using shot data in the model"))
  }
  
  return(df_football_data)
}
