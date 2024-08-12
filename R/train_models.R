train_models <- function(input_data, namen, aggregated_football_data_cache, run_number, competition_parameters) {
  flog.info("Starts training prediction models based on data from past seasons")
  
  football_data <- input_data$football_data
  aggregated_football_data <- input_data$aggregated_football_data
  aggregated_level_two_data <-  input_data$aggregated_level_two_data
  aggregated_level_two_data[setdiff(colnames(aggregated_football_data), colnames(aggregated_level_two_data))] <- NA
  aggregated_level_two_data <- aggregated_level_two_data[colnames(aggregated_football_data)]
  aggregated_football_data <- rbind(aggregated_football_data, aggregated_level_two_data) %>%
    mutate(Seizoen = as.integer(Seizoen))
   
  aggregated_transfermarkt_data <- input_data$aggregated_transfermarkt_data
  
  all_models <- list()
  for(game_round in 0 : 30) {
    flog.info(paste0("Start training model for game round ", game_round))
    if(game_round > 0) {
      predicted_outcome <- data.frame(Team = model_input$Team,
                                      Competitie = model_input$Competitie,
                                      Seizoen = model_input$Seizoen,
                                      Punten = ifelse(is.na(model_input$Schotsaldo_vorig_seizoen), 
                                                      predict(all_models[[game_round]]$punten$without_shots, newdata = model_input), 
                                                      predict(all_models[[game_round]]$punten$with_shots, newdata = model_input)),
                                      Goals = ifelse(is.na(model_input$Schotsaldo_vorig_seizoen), 
                                                     predict(all_models[[game_round]]$goals$without_shots, newdata = model_input), 
                                                     predict(all_models[[game_round]]$goals$with_shots, newdata = model_input)))
      goal_expectations <- calculate_goal_expectations(predicted_outcome, competition_parameters$points_to_goalratio) %>%
        mutate(Team = mgsub(as.character(predicted_outcome$Team), as.character(namen$Transfermarkt), as.character(namen$Football_data), fixed = TRUE))
      match_expectations <- calculate_match_expectations(football_data[seq(1, nrow(football_data), by = 2), ], goal_expectations, 1.35, competition_parameters$home_advantage) %>%
        mutate(HomePoints = Home_prob * 3 + Draw_prob,
               AwayPoints = Away_prob * 3 + Draw_prob,
               Goals = (ExpHG + ExpAG) / 1.35) %>%
        select(HomeTeam, AwayTeam, Wedstrijdnummer, Competitie, Seizoen, HomePoints, AwayPoints, Goals)
      
      football_data_game_round <- football_data %>%
        full_join(match_expectations, by = c("HomeTeam", "AwayTeam", "Wedstrijdnummer", "Competitie", "Seizoen"))
      football_data_game_round$HomePoints[seq(2, nrow(football_data_game_round), by = 2)] <- football_data_game_round$AwayPoints[seq(1, nrow(football_data_game_round), by = 2)]
      football_data_game_round$AwayPoints[seq(2, nrow(football_data_game_round), by = 2)] <- football_data_game_round$HomePoints[seq(1, nrow(football_data_game_round), by = 2)]
      football_data_game_round$Goals[seq(2, nrow(football_data_game_round), by = 2)] <- football_data_game_round$Goals[seq(1, nrow(football_data_game_round), by = 2)]
      
      football_data_to_come <- football_data_game_round %>%
        filter(Aantal > game_round)
      aggregated_football_data_to_come <- use_function_with_caching(aggregated_football_data_cache,
                                                                    paste0("aggregated_football_data_to_come_game_round_", game_round),
                                                                    run_number,
                                                                    aggregate_football_data,
                                                                    football_data_to_come,
                                                                    namen) %>%
        mutate(Punten = Punten + calculate_predicted_outcomes(football_data_game_round)$Points - calculate_predicted_outcomes(football_data_to_come)$Points,
               Doelsom = Doelsom + calculate_predicted_outcomes(football_data_game_round)$Goals - calculate_predicted_outcomes(football_data_to_come)$Goals)
      model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data_to_come, is_new_data = FALSE)
      
      football_data_passed <- football_data_game_round %>%
        filter(Aantal <= game_round)
      aggregated_football_data_passed <- use_function_with_caching(aggregated_football_data_cache,
                                                                   paste0("aggregated_football_data_passed_game_round_", game_round),
                                                                   run_number,
                                                                   aggregate_football_data,
                                                                   football_data_passed,
                                                                   namen) %>%
        mutate(Schedule_points = calculate_predicted_outcomes(football_data_game_round)$Points - calculate_predicted_outcomes(football_data_passed)$Points,
               Schedule_goals = calculate_predicted_outcomes(football_data_game_round)$Goals - calculate_predicted_outcomes(football_data_passed)$Goals)
      model_input <- add_in_season_info(model_input, aggregated_football_data_passed) %>%
        select(-Aantalwedstrijden)
      mean_games <- mean(right_join(aggregated_football_data_to_come, model_input, by = c("Team", "Competitie", "Seizoen"))$Aantalwedstrijden) + game_round
      model_with_old_params <- create_models_for_game_round(model_input, mean_games - game_round, all_models[[game_round]], threshold = 0.05)
      all_models[[game_round + 1]] <- create_models_for_game_round(model_input, mean_games - game_round, fixed_model = model_with_old_params)
      browser()
    } else {
      model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data, is_new_data = FALSE)
      mean_games <- mean(right_join(aggregated_football_data, model_input, by = c("Team", "Competitie", "Seizoen"))$Aantalwedstrijden) + game_round
      all_models[[game_round + 1]] <- create_models_for_game_round(model_input, mean_games)
      } 
    flog.info(paste0("Created model for game round ", game_round))
  }
  save(all_models, file = paste0("cache/all_models_",
                                 Sys.Date(),
                                 ".RData"))
  return(all_models)
}