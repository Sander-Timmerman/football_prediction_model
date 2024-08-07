gather_level_two_final_standings <- function(data_source_info, is_current_season) {
  urls_tm <- find_data_urls(data_source_info, "transfermarkt", is_current_season, 24, "tabelle", 2)
  all_final_standings <- data.frame()
  for(i in seq_len(nrow(urls_tm))) {
    competition_url <- as.character(urls_tm[i, 4])
    webpage <- read_url(competition_url, use_rvest = TRUE, stop_if_failed = TRUE)
    club_urls <- webpage %>% html_nodes("#yw1 .hauptlink a:nth-child(1)") %>% html_attr("href")
    club_urls <- paste0("http://www.transfermarkt.com", gsub("startseite", "kader", club_urls), "/plus/1")
    all_club_names <- character(0)
    for(club_url in club_urls) {
      webpage_club <- read_url(club_url, use_rvest = TRUE, stop_if_failed = TRUE)
      club_name <- html_nodes(webpage_club, ".data-header__headline-wrapper--oswald") %>% html_text()
      club_name <- substr(club_name, 14, nchar(club_name) - 8)
      all_club_names <- c(all_club_names, club_name)
    }
    standings_data <- html_nodes(webpage, ".zentriert:nth-child(10) , td:nth-child(8) , #yw1 .no-border-links+ .zentriert") %>%
      html_text()
    n_matches <- as.numeric(standings_data[seq(1, by = 3, length.out = length(all_club_names))])
    points <- as.numeric(standings_data[seq(3, by = 3, length.out = length(all_club_names))])
    goal_numbers <- strsplit(standings_data[seq(2, by = 3, length.out = length(all_club_names))], ":")
    goals_for <- as.numeric(sapply(goal_numbers, function(x) x[1]))
    goals_against <- as.numeric(sapply(goal_numbers, function(x) x[2]))
    final_standings <- data.frame(Team = all_club_names,
                                  Competitie = as.character(urls_tm[i, 1]),
                                  Seizoen = 24,
                                  Aantalwedstrijden = n_matches,
                                  Punten = points,
                                  Doelpuntenvoor = goals_for,
                                  Doelpuntentegen = goals_against)
    all_final_standings <- rbind(all_final_standings, final_standings)
  }
  return(all_final_standings)
}

