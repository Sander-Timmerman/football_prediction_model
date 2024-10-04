create_current_season_prediction <- function(aggregated_football_data_old, input_data_new, all_models, write_results, run_number, competition_parameters, namen) {
  flog.info("Using models to make a prediction for current season")
  
  football_data_new <- input_data_new$football_data
  aggregated_football_data_new <- input_data_new$aggregated_football_data
  aggregated_transfermarkt_data_new <- input_data_new$aggregated_transfermarkt_data

  aggregated_football_data_with_shots <- aggregated_football_data_old[!is.na(aggregated_football_data_old$Schotsaldo), ]
  aggregated_football_data_without_shots <- aggregated_football_data_old[is.na(aggregated_football_data_old$Schotsaldo), ] %>%
    mutate(Doelpuntenvoor = NA,
           Doelpuntentegen = NA) %>%
    select(Team, Competitie, Seizoen, Aantalwedstrijden, Punten, Doelpuntenvoor, Doelpuntentegen, Niveau, Doelsaldo, Goalratio, Doelsom)
  
  model_input <- create_model_input(aggregated_football_data_old, aggregated_transfermarkt_data_new, is_new_data = TRUE)
  prediction <- data.frame(Team = model_input$Team,
                           Competitie = model_input$Competitie,
                           Seizoen = model_input$Seizoen,
                           Punten = ifelse(is.na(model_input$Schotsaldo_vorig_seizoen), 
                                           predict(all_models[[1]]$punten$without_shots, newdata = model_input), 
                                           predict(all_models[[1]]$punten$with_shots, newdata = model_input)),
                           Punten_sd = ifelse(is.na(model_input$Schotsaldo_vorig_seizoen),
                                              all_models[[1]]$punten$without_shots$performance,
                                              all_models[[1]]$punten$with_shots$performance),
                           Goals = ifelse(is.na(model_input$Schotsaldo_vorig_seizoen), 
                                          predict(all_models[[1]]$goals$without_shots, newdata = model_input), 
                                          predict(all_models[[1]]$goals$with_shots, newdata = model_input)),
                           Goals_sd = ifelse(is.na(model_input$Schotsaldo_vorig_seizoen),
                                             all_models[[1]]$goals$without_shots$performance,
                                             all_models[[1]]$goals$with_shots$performance))
  last_prediction <- prediction
  all_predictions <- prediction
  all_matches <- model_input %>%
    group_by(Competitie, Seizoen) %>%
    do({
      team_combinations <- expand.grid(HomeTeam = .$Team, AwayTeam = .$Team)
      team_combinations <- team_combinations %>%
        filter(HomeTeam != AwayTeam)
      team_combinations
    }) %>%
    ungroup()
  for(game_round in seq_len(max(football_data_new$Aantalwedstrijden))) {
    goal_expectations <- calculate_goal_expectations(last_prediction, competition_parameters$points_to_goalratio)
    match_expectations <- calculate_match_expectations(all_matches, goal_expectations, 1.35, competition_parameters$home_advantage) %>%
      mutate(HomePoints = Home_prob * 3 + Draw_prob,
             AwayPoints = Away_prob * 3 + Draw_prob,
             Goals = (ExpHG + ExpAG) / 1.35)
    match_expectations <- rbind(match_expectations,
                                mutate(match_expectations, 
                                       HomeTeam = match_expectations$AwayTeam, 
                                       AwayTeam = match_expectations$HomeTeam,
                                       HomePoints = match_expectations$AwayPoints,
                                       AwayPoints = match_expectations$HomePoints))
    predicted_outcome <- calculate_predicted_outcomes(match_expectations)
    
    football_data_game_round <- football_data_new %>%
      mutate(Home_away = row_number() %% 2) %>%
      filter(Aantal <= game_round)
    goal_expectations <- goal_expectations %>%
      mutate(Team = mgsub(as.character(last_prediction$Team), as.character(namen$Transfermarkt), as.character(namen$Football_data), fixed = TRUE))
    match_expectations <- football_data_game_round %>%
      filter(Home_away == 1) %>% 
      calculate_match_expectations(goal_expectations, 1.35, competition_parameters$home_advantage) %>%
      mutate(HomePoints = Home_prob * 3 + Draw_prob,
             AwayPoints = Away_prob * 3 + Draw_prob,
             Goals = (ExpHG + ExpAG) / 1.35) %>%
      select(HomeTeam, AwayTeam, Wedstrijdnummer, Competitie, Seizoen, HomePoints, AwayPoints, Goals)
    football_data_game_round <- football_data_game_round %>%
      full_join(match_expectations, by = c("HomeTeam", "AwayTeam", "Wedstrijdnummer", "Competitie", "Seizoen"))
    match_expectations <- football_data_game_round %>%
      filter(Home_away == 0) %>% 
      calculate_match_expectations(goal_expectations, 1.35, 2 - competition_parameters$home_advantage) %>%
      mutate(HomePoints = Home_prob * 3 + Draw_prob,
             AwayPoints = Away_prob * 3 + Draw_prob,
             Goals = (ExpHG + ExpAG) / 1.35) %>%
      select(HomeTeam, AwayTeam, Wedstrijdnummer, Competitie, Seizoen, HomePoints, AwayPoints, Goals)
    football_data_game_round <- football_data_game_round %>%
      full_join(match_expectations, by = c("HomeTeam", "AwayTeam", "Wedstrijdnummer", "Competitie", "Seizoen")) %>%
      mutate(HomePoints = coalesce(HomePoints.x, HomePoints.y),
             AwayPoints = coalesce(AwayPoints.x, AwayPoints.y),
             Goals = coalesce(Goals.x, Goals.y)) %>%
      select(-(HomePoints.x : Goals.y))
    
    predicted_outcome_passed <- calculate_predicted_outcomes(football_data_game_round) %>%
      mutate(Team = mgsub(as.character(Team), namen$Football_data, namen$Transfermarkt))
    aggregated_football_data_passed <- aggregate_football_data(football_data_game_round, namen) %>%
      left_join(predicted_outcome, by = c("Team", "Seizoen")) %>%
      left_join(predicted_outcome_passed, by = c("Team", "Seizoen")) %>%
      mutate(Schedule_points = Points.x - Points.y,
             Schedule_goals = Goals.x - Goals.y) %>%
      select(-(Points.x : Goals.y))
    model_input_game_round <- model_input %>%
      add_in_season_info(aggregated_football_data_passed) %>%
      filter(Aantalwedstrijden == game_round)
    prediction <- data.frame(Team = model_input_game_round$Team,
                             Competitie = model_input_game_round$Competitie,
                             Seizoen = model_input_game_round$Seizoen,
                             Punten = ifelse(is.na(model_input_game_round$Schotsaldo_vorig_seizoen), 
                                             predict(all_models[[game_round + 1]]$punten$without_shots, newdata = model_input_game_round), 
                                             predict(all_models[[game_round + 1]]$punten$with_shots, newdata = model_input_game_round)),
                             Punten_sd = ifelse(is.na(model_input_game_round$Schotsaldo_vorig_seizoen),
                                                all_models[[game_round + 1]]$punten$without_shots$performance,
                                                all_models[[game_round + 1]]$punten$with_shots$performance),
                             Goals = ifelse(is.na(model_input_game_round$Schotsaldo_vorig_seizoen), 
                                            predict(all_models[[game_round + 1]]$goals$without_shots, newdata = model_input_game_round), 
                                            predict(all_models[[game_round + 1]]$goals$with_shots, newdata = model_input_game_round)),
                             Goals_sd = ifelse(is.na(model_input_game_round$Schotsaldo_vorig_seizoen),
                                               all_models[[game_round + 1]]$goals$without_shots$performance,
                                               all_models[[game_round + 1]]$goals$with_shots$performance))

    if(any(is.na(prediction))) {
      flog.warn(paste0("There are NA values in game round ", 
                       game_round, 
                       " for the following teams: ",
                       paste0(prediction$Team[which(!complete.cases(prediction))], collapse = ", "),
                       ". The prediction based on this game round will be ignored"))
      prediction <- na.omit(prediction)
    }
    
    all_predictions <- rbind(all_predictions, prediction)
    last_prediction <- all_predictions %>%
      group_by(Team) %>%
      slice_tail(n = 1) %>%
      ungroup()
  }
  
  if(write_results) {
    write.xlsx(last_prediction, file.path("output", run_number, paste0("ratings.xlsx")))
    flog.info(paste0("Written predicted future points and goals in the output folder"))
  }
  
  return(last_prediction)
}