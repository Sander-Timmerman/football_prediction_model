read_football_data_csv <- function(url, competition, season) {
  df_football_data <- tryCatch({
    csv <- suppressWarnings(read.csv(url, stringsAsFactors = FALSE))
    flog.debug(paste0("Succesfully read football_data csv with url ", url))
    return(csv)
  },
    error = function(cond) {
      if(format(Sys.Date(), "%m") %in% c("06", "07", "08") & season == 25) {
        flog.info(paste0("Competition ",
                         competition,
                         " seems to not have started yet, so no new football data available"))
        } else {
          flog.warn(paste0("Url ",
                           url,
                           " has no football_data available, returning empty data frame"))
          }
      return(data.frame(HomeTeam = character(0), 
                        AwayTeam = character(0), 
                        FTHG = numeric(0), 
                        FTAG = numeric(0), 
                        FTR = character(0), 
                        HS = numeric(0), 
                        AS = numeric(0), 
                        HST = numeric(0), 
                        AST = numeric(0)))
    }
  )
  return(df_football_data)
}