read_football_data_csv <- function(url, competition) {
  df_football_data <- tryCatch(
    suppressWarnings(read.csv(url)),
    error = function(cond) {
      if(format(Sys.Date(), "%m") %in% c("06", "07", "08")) {
        flog.info(paste0("Competition ",
                         competition,
                         " seems to not have started yet, so no new football data available"))
        } else {
          flog.warn(paste0("Competition ",
                           competition,
                           " has no new football_data available, running as if the competition hasn't started yet"))
          }
      return(data.frame())
    }
  )
  return(df_football_data)
}