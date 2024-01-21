determine_market_value <- function(json, datum_ref) {
  waardes_list <- fromJSON(json)$list
  if(length(waardes_list) == 0) return(0)
  
  marktwaardes <- NULL
  datums <- NULL
  
  for(data_point in waardes_list) {
    marktwaarde <- data_point$y
    marktwaardes <- c(marktwaardes, marktwaarde)
    
    datum <- data_point$datum_mw
    maand <- substr(datum, 1, 3)
    maanden <- read.csv("maanden.csv", colClasses = c(rep("character", 2)))
    maand <- mgsub(maand, maanden$Maand, maanden$Cijfer)
    
    komma <- gregexpr(",", datum, fixed = TRUE)[[1]]
    dag <- gsub(" ", "0", substr(datum, komma - 2, komma - 1), fixed = TRUE)
    jaar <- substr(datum, komma + 2, komma + 5)
    datum <- paste0(dag, maand, jaar)
    datums <- c(datums, datum)
  }
  df_marktwaardes <- data.frame(Marktwaarde = marktwaardes,
                                Datum = as.Date(datums, "%d%m%Y"))
  marktwaarde_ref <- df_marktwaardes[sum(datum_ref > df_marktwaardes$Datum), "Marktwaarde"]
  if(length(marktwaarde_ref) == 0) marktwaarde_ref <- 0
  
  return(marktwaarde_ref)
}