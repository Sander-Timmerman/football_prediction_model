train_models <- function(football_data, aggregated_football_data, aggregated_transfermarkt_data) {
  flog.info("Starts training prediction models based on data from previous seasons")
  all_models <- list()
  for(game_round in 0 : 30) {
    if(game_round > 0) {
      football_data_to_come <- football_data %>%
        filter(Aantal > game_round)
      aggregated_football_data_to_come <- aggregate_football_data(football_data_to_come)
      model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data_to_come, is_new_data = FALSE)
      
      football_data_passed <- football_data %>%
        filter(Aantal <= game_round)
      aggregated_football_data_passed <- aggregate_football_data(football_data_passed)
      model_input <- add_in_season_info(model_input, aggregated_football_data_passed)
    } else model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data, is_new_data = FALSE)
    
    all_models[[game_round + 1]] <- create_models_for_game_round(model_input)
  }
  save(all_models, file = paste0("cache/all_models_",
                                 Sys.Date(),
                                 ".RData"))
  return(all_models)
}