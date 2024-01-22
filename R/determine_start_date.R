determine_start_date <- function(df_football_data) {
  date_format <- switch(as.character(nchar(as.character(df_football_data$Date)[1])),
                        "10" = "%d/%m/%Y",
                        "8" = "%d/%m/%y")
  if(is.null(date_format)) {
    return(NA)
  } 
  
  start_date <- min(as.Date(df_football_data$Date, date_format), na.rm = TRUE)
  return(start_date)
}