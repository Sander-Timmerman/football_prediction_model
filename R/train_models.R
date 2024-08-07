train_models <- function(input_data, namen, aggregated_football_data_cache, run_number) {
  flog.info("Starts training prediction models based on data from past seasons")
  
  football_data <- input_data$football_data
  aggregated_football_data <- input_data$aggregated_football_data
  aggregated_transfermarkt_data <- input_data$aggregated_transfermarkt_data
  
  all_models <- list()
  all_means <- numeric()
  for(game_round in 0 : 30) {
    flog.info(paste0("Start training model for game round ", game_round))
    if(game_round > 0) {
      football_data_to_come <- football_data %>%
        filter(Aantal > game_round)
      aggregated_football_data_to_come <- use_function_with_caching(aggregated_football_data_cache,
                                                                    paste0("aggregated_football_data_to_come_game_round_", game_round),
                                                                    run_number,
                                                                    aggregate_football_data,
                                                                    football_data_to_come,
                                                                    namen)
      model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data_to_come, is_new_data = FALSE)
      
      football_data_passed <- football_data %>%
        filter(Aantal <= game_round)
      aggregated_football_data_passed <- use_function_with_caching(aggregated_football_data_cache,
                                                                   paste0("aggregated_football_data_passed_game_round_", game_round),
                                                                   run_number,
                                                                   aggregate_football_data,
                                                                   football_data_passed,
                                                                   namen)
      model_input <- add_in_season_info(model_input, aggregated_football_data_passed) %>%
        select(-Aantalwedstrijden)
      mean_match_numbers <- mean(right_join(aggregated_football_data_to_come, model_input, by = c("Team", "Competitie", "Seizoen"))$Aantalwedstrijden) + game_round
      all_means <- c(all_means, mean_match_numbers)
    } else {
      model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data, is_new_data = FALSE)
      browser()
      mean_match_numbers <- mean(right_join(aggregated_football_data, model_input, by = c("Team", "Competitie", "Seizoen"))$Aantalwedstrijden) + game_round
      all_means <- c(all_means, mean_match_numbers)
    } 
    all_models[[game_round + 1]] <- create_models_for_game_round(model_input)
    flog.info(paste0("Created model for game round ", game_round))
  }
  saveRDS(all_means, file = "all_means.rds")
  save(all_models, file = paste0("cache/all_models_",
                                 Sys.Date(),
                                 ".RData"))
  return(all_models)
}