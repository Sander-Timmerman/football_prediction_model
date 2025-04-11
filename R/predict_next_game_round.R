predict_next_game_round <- function(prediction, data_source_info, settings, blogger_info, run_number, competition_parameters, maanden) {
  flog.info("Starts prediction for next game round")
  urls_tm <- find_data_urls(data_source_info, "transfermarkt", is_current_season = TRUE, settings$current_season, "spieltagtabelle")
  home_teams <- character()
  away_teams <- character()
  competitions <- character()
  all_dates <- character()

  for(row in seq_len(nrow(urls_tm))) {
    webpage <- read_url(as.character(urls_tm[row, 4]), use_rvest = TRUE)
    if(is.null(webpage)) next
    matches <- html_nodes(webpage, ".hauptlink.hide-for-small a") %>%
      html_text()
    dates <- html_nodes(webpage, "td.hide-for-small:nth-child(1)") %>%
      html_text() %>%
      substr(118, 129) %>%
      sub("\\s+$", "", .) %>%
      replace(. == "", NA) %>%
      na.locf()
    if(length(matches) %% 2 != 0) next
    home_teams <- c(home_teams, matches[seq(1, length(matches) - 1, 2)])
    away_teams <- c(away_teams, matches[seq(2, length(matches), 2)])
    competitions <- c(competitions, rep(as.character(urls_tm[row, 1]), length(matches) / 2))
    all_dates <- c(all_dates, dates)
    if(length(competitions) != length(all_dates)) browser()
  }
  
  if(sum(length(home_teams), length(away_teams), length(competitions)) == 0) {
    flog.error("No valid upcoming matches have been found at all, no output for next game round prediction")
    return(NULL)
  }
  
  df_matches <- data.frame(HomeTeam = home_teams,
                           AwayTeam = away_teams,
                           Competition = competitions,
                           Date = all_dates,
                           Seizoen = settings$current_season) %>%
    mutate(Date = parse_date_from_transfermarkt(Date, maanden)) %>%
    left_join(data.frame(Competition = names(competition_parameters$goals_per_competition), 
                         Goals_per_match = competition_parameters$goals_per_competition), 
              by = "Competition")
  
  df_matches_football_data <- read.csv("https://www.football-data.co.uk/fixtures.csv") %>%
    filter(Div %in% paste0(data_source_info$Code_football_data, c(0, rep(1, nrow(data_source_info) - 1))))
  
  goal_expectations <- calculate_goal_expectations(prediction, competition_parameters$points_to_goalratio)
  match_expectations <- calculate_match_expectations(df_matches, goal_expectations, df_matches$Goals_per_match, competition_parameters$home_advantage) %>%
    select(-c(Goals_per_match, Seizoen))
  colnames(match_expectations) <- c("Thuisploeg", "Uitploeg", "Competitie", "Thuisgoals", "Uitgoals", "Thuiswinst", "Gelijk", "Uitwinst")
  flog.info("Calculated prediction for next game round")
  
  if(settings$write_results) {
    tryCatch(
      {
        write.xlsx(match_expectations, file.path("output", run_number, paste0("match_expectations.xlsx")))
        save_match_expectations_as_html(match_expectations, blogger_info, settings$edit_blogger, file.path("output", run_number, paste0("match_expectations.html")))
        flog.info(paste0("Written match expectations in the output folder"))
      },
      error = function(e) {
        flog.error(paste0("Writing output for match expectations failed. Prediction is only stored in the R output. Error message: ", e))
      }
    )
  }
  return(match_expectations)
}