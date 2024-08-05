predict_next_game_round <- function(prediction) {
  flog.info("Starts prediction for next game round")
  webpage <- read_url("https://www.transfermarkt.com/eredivisie/spieltagtabelle/wettbewerb/NL1/saison_id/2023", use_rvest = TRUE)
  if(is.null(webpage)) return(data.frame())
  matches <- html_nodes(webpage, ".hauptlink.hide-for-small a") %>%
    html_text()
  df_matches <- data.frame(HomeTeam = matches[seq(1, length(matches) - 1, 2)],
                           AwayTeam = matches[seq(2, length(matches), 2)])
  goal_expectations <- calculate_goal_expectations(prediction)
  match_expectations <- calculate_match_expectations(df_matches, goal_expectations, 1.5)
  return(match_expectations)
}