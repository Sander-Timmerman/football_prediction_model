predict_next_game_round <- function(prediction, data_source_info, settings, run_number) {
  flog.info("Starts prediction for next game round")
  urls_tm <- find_data_urls(data_source_info, "transfermarkt", is_current_season = TRUE, settings$current_season, "spieltagtabelle")
  home_teams <- character()
  away_teams <- character()
  for(url in urls_tm$Url) {
    webpage <- read_url(url, use_rvest = TRUE)
    if(is.null(webpage)) next
    matches <- html_nodes(webpage, ".hauptlink.hide-for-small a") %>%
      html_text()
    home_teams <- c(home_teams, matches[seq(1, length(matches) - 1, 2)])
    away_teams <- c(away_teams, matches[seq(2, length(matches), 2)])
  }
  df_matches <- data.frame(HomeTeam = home_teams,
                           AwayTeam = away_teams)
  goal_expectations <- calculate_goal_expectations(prediction)
  match_expectations <- calculate_match_expectations(df_matches, goal_expectations, 1.5)
  if(settings$write_results) {
    write.xlsx(match_expectations, file.path("output", run_number, paste0("match_expectations.xlsx")))
    flog.info(paste0("Written match expectations in the output folder"))
  }
  return(match_expectations)
}