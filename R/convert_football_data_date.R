convert_football_data_date <- function(df_football_data, competition, season, level) {
  date_format <- switch(as.character(nchar(as.character(df_football_data$Date)[1])),
                        "10" = "%d/%m/%Y",
                        "8" = "%d/%m/%y")
  if(is.null(date_format)) {
    flog.error(paste0("Unknown date format in football_data csv for competition ",
                      competition,
                      ", season ",
                      season,
                      ", level ",
                      level))
    return(data.frame())
  }
  
  df_football_data <- df_football_data %>%
    mutate(Date = as.Date(Date, date_format)) %>%
    arrange(Date)

  return(df_football_data)
}