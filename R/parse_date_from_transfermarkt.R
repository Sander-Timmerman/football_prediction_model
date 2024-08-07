parse_date_from_transfermarkt <- function(date_string, maanden) {
  date_string[date_string %in% c("-", "")] <- "Jan 1, 1900"
  jaar <- as.numeric(substr(date_string, nchar(date_string) - 3, nchar(date_string)))
  
  maand <- substr(date_string, 1, 3)
  maand <- mgsub(maand, maanden$Maand, maanden$Cijfer)
  
  dag <- as.numeric(substr(date_string, 5, nchar(date_string) - 6))
  
  if(any(is.na(jaar)) | any(is.na(maand)) | any(is.na(dag))) {
    flog.warn("Unknown date format in Transfermarkt data. Giving those a default of 1 January 1900")
    jaar[is.na(jaar)] <- 1900
    maand[is.na(maand)] <- 1
    dag[is.na(dag)] <- 1
  }
  date <- as.Date(paste(jaar, maand, dag, sep = "-"))
  
  return(date)
} 