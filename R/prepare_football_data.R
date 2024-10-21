prepare_football_data <- function(df_football_data, competitie, seizoen, level, local_input) {
  df_football_data <- df_football_data[nchar(df_football_data[[1]]) > 0, ]
  
  if(is.null(df_football_data$HS)) {
    df_football_data$HS <- NA
    df_football_data$AS <- NA
    df_football_data$HST <- NA
    df_football_data$AST <- NA
  }
  
  df_football_data <- df_football_data %>%
    add_additional_football_data(local_input$additional_football_data, competitie, seizoen, level) %>%
    convert_football_data_date(competitie, seizoen, level)
  
  if(any((!df_football_data$HomeTeam %in% local_input$names$Football_data |
          !df_football_data$AwayTeam %in% local_input$names$Football_data) &
         level == 1)) {
    flog.warn(paste0("At least one team has an unknown name in football_data for competition ", 
                     competitie,
                     " and season ", 
                     seizoen,
                     ". This might cause problems when joining with Transfermarkt data"))
  }
  
  return(df_football_data)
}
