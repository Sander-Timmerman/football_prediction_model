parse_date_from_transfermarkt <- function(date_string) {
  jaar <- as.numeric(substr(date_string, nchar(date_string) - 3, nchar(date_string)))
  jaar[is.na(jaar)] <- 1900
  
  maand <- substr(date_string, 1, 3)
  maanden <- read.csv("input/maanden.csv", colClasses = c(rep("character", 2)))
  maand <- mgsub(maand, maanden$Maand, maanden$Cijfer)
  maand[is.na(maand)] <- 1
  
  dag <- as.numeric(substr(date_string, 5, nchar(date_string) - 6))
  dag[is.na(dag)] <- 1
  
  date <- as.Date(paste(jaar, maand, dag, sep="-"))
  return(date)
} 