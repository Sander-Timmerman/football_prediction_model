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
      return(data.frame())
    }
  )
  return(df_football_data)
}