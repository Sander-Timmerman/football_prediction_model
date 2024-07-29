find_data_urls <- function(data_source_info, source, is_current_season) {
  if(!source %in% c("football_data", "transfermarkt")) {
    flog.error(paste0("Data source ", source, " is unknown, returning empty dataframe"))
    return(data.frame())
  }
  if(any(data_source_info$Start >= 25)) {
    flog.warn(paste0("Invalid start season in competition ", 
                     data_source_info[which(data_source_info$Start >= 25), "Competitie"],
                     ". This competition will be ignored"))
    data_source_info <- filter(data_source_info, Start < 25)
  }
  all_competitions <- character(0)
  all_seasons <- integer(0)
  all_levels <- integer(0)
  all_urls <- character(0)
  
  for(row in seq_len(nrow(data_source_info))) {
    competition <- data_source_info[row, "Competitie"]
    code <- data_source_info[row, paste0("Code_", source)]
    start_season <- if(is_current_season) 25 else data_source_info[row, "Start"]
    end_season <- if(is_current_season) 25 else 24
    for(season in start_season : end_season) {
      for(level in 1 : 2) {
         url <- if(source == "football_data") {
           paste0("https://www.football-data.co.uk/mmz4281/", 
                  season - 1, 
                  season,
                  "/",
                  code,
                  if(competition == "Engeland") level - 1 else level,
                  ".csv")
         } else {
           paste0("https://www.transfermarkt.com/jumplist/startseite/wettbewerb/",
                  code,
                  "1/plus/?saison_id=20",
                  season - 1) 
         }
        all_competitions <- c(all_competitions, competition)
        all_seasons <- c(all_seasons, season)
        all_levels <- c(all_levels, level)
        all_urls <- c(all_urls, url)
        
        level_two <- data_source_info[row, "Twee"]
        if(!level_two | source == "transfermarkt" | is_current_season) break
      }
    }
  }
  urls_fd <- data.frame(Competitie = all_competitions,
                        Seizoen = all_seasons,
                        Niveau = all_levels,
                        Url = all_urls)
  
  return(urls_fd)
}