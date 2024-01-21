gather_transfermarkt_data <- function(urls_tm, player_jsons = list(), current_season) {
  transfermarkt_data <- NULL
  for(i in seq_len(nrow(urls_tm))) {
    competition_urls <- as.character(urls_tm[i, 3])
    start_date <- urls_tm[i, "Startdatum"]
    flog.info(paste0("Start met competitie ", as.character(urls_tm[i,1]), ", seizoen ", as.character(urls_tm[i,2])))
    webpage <- read_url(competition_urls, use_rvest = TRUE)
    club_urls <- html_nodes(webpage, "#yw1 .no-border-links a:nth-child(1)") %>% html_attr("href")
    club_urls <- paste0("http://www.transfermarkt.com", gsub("startseite", "kader", club_urls), "/plus/1")
    for(club_url in club_urls) {
      webpage_club <- read_url(club_url, use_rvest = TRUE)
      club_name <- html_nodes(webpage_club, ".data-header__headline-wrapper--oswald") %>% html_text()
      club_name <- substr(club_name, 14, nchar(club_name) - 8)
      flog.info(paste0("Start met club ", club_name))
      player_names <- html_nodes(webpage_club, ".inline-table a") %>% html_text()
      player_urls <- html_nodes(webpage_club, ".inline-table a") %>% html_attr("href")
      slashes <- unlist(gregexpr("/", player_urls, fixed = TRUE))[seq(4, length(player_urls) * 4, 4)]
      player_ids <- as.character(substr(player_urls, slashes + 1, nchar(player_urls)))
      date_column <- ifelse(current_season, "td:nth-child(7)", "td:nth-child(8)")
      date_string <- html_nodes(webpage_club, date_column) %>%
        html_text() %>%
        parse_date_from_transfermarkt()
      if(current_season) {
        market_values <- html_nodes(webpage_club, ".rechts.hauptlink") %>%
          html_text() %>%
          convert_market_value_to_number()
      } else {
        market_values <- NULL
        for(player_id in player_ids) {
          if(is.null(player_jsons[[player_id]])) {
            player_jsons[[player_id]] <- read_url(url = paste0("https://www.transfermarkt.com/ceapi/marketValueDevelopment/graph/", player_id),
                                                  use_rvest = FALSE)
            if(player_jsons[[player_id]] == "error") save(player_jsons, file = paste0("player_jsons_",
                                                                                      Sys.Date(),
                                                                                      ".RData"))
          }
          market_value <- determine_market_value(player_jsons[[player_id]], start_date)
          market_values <- c(market_values, market_value)
        }
      }
      temp_transfermarkt_data <- data.frame(Speler = player_names,
                                            Datum_gejoind = date_string, 
                                            Marktwaarde = market_values, 
                                            Team = club_name, 
                                            Competitie = urls_tm[i, 1], 
                                            Seizoen = urls_tm[i, 2]) %>% 
        filter(Datum_gejoind < start_date)
      transfermarkt_data <- rbind(transfermarkt_data, temp_transfermarkt_data)
    }
  }
  if(current_season) {
    save(player_jsons, file = paste0("cache/player_jsons_",
                                     Sys.Date(),
                                     ".RData"))
  }
  return(transfermarkt_data)
}