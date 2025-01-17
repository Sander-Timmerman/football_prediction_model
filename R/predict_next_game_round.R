predict_next_game_round <- function(prediction, data_source_info, settings, run_number, competition_parameters) {
  flog.info("Starts prediction for next game round")
  urls_tm <- find_data_urls(data_source_info, "transfermarkt", is_current_season = TRUE, settings$current_season, "spieltagtabelle")
  home_teams <- character()
  away_teams <- character()
  competitions <- character()

  for(row in seq_len(nrow(urls_tm))) {
    webpage <- read_url(as.character(urls_tm[row, 4]), use_rvest = TRUE)
    if(is.null(webpage)) next
    matches <- html_nodes(webpage, ".hauptlink.hide-for-small a") %>%
      html_text()
    home_teams <- c(home_teams, matches[seq(1, length(matches) - 1, 2)])
    away_teams <- c(away_teams, matches[seq(2, length(matches), 2)])
    competitions <- c(competitions, rep(as.character(urls_tm[row, 1]), length(matches) / 2))
  }
  
  df_matches <- data.frame(HomeTeam = home_teams,
                           AwayTeam = away_teams,
                           Competition = competitions,
                           Seizoen = settings$current_season) %>%
    left_join(data.frame(Competition = names(competition_parameters$goals_per_competition), 
                         Goals_per_match = competition_parameters$goals_per_competition), 
              by = "Competition")
  goal_expectations <- calculate_goal_expectations(prediction, competition_parameters$points_to_goalratio)
  match_expectations <- calculate_match_expectations(df_matches, goal_expectations, df_matches$Goals_per_match, competition_parameters$home_advantage) %>%
    select(-c(Goals_per_match, Seizoen))
  colnames(match_expectations) <- c("Thuisploeg", "Uitploeg", "Competitie", "Thuisgoals", "Uitgoals", "Thuiswinst", "Gelijk", "Uitwinst")
  
  if(settings$write_results) {
    write.xlsx(match_expectations, file.path("output", run_number, paste0("match_expectations.xlsx")))
    save_match_expectations_as_html(match_expectations, file.path("output", run_number, paste0("match_expectations.html")))
    flog.info(paste0("Written match expectations in the output folder"))
  }
  return(match_expectations)
}