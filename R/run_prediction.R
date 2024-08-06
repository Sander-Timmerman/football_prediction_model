run_prediction <- function(all_cache_numbers, local_input, settings, run_number) {
  
  input_data_past_seasons <- load_input_data(all_cache_numbers$football_data_cache, 
                                             all_cache_numbers$aggregated_football_data_cache, 
                                             all_cache_numbers$transfermarkt_data_cache, 
                                             all_cache_numbers$player_jsons_cache,
                                             all_final_standings_cache = NULL,
                                             run_number,
                                             local_input,
                                             is_current_season = FALSE,
                                             settings$current_season)
  
  all_models <- use_function_with_caching(all_cache_numbers$all_models_cache, 
                                          "all_models",
                                          run_number,
                                          train_models,
                                          input_data_past_seasons, 
                                          local_input$names)
  
  input_data_this_season <- load_input_data(football_data_cache = NULL, 
                                            aggregated_football_data_cache = NULL, 
                                            transfermarkt_data_cache = all_cache_numbers$transfermarkt_data_new_cache, 
                                            player_jsons_cache = NULL,
                                            all_cache_numbers$all_final_standings_cache,
                                            run_number,
                                            local_input,
                                            is_current_season = TRUE,
                                            settings$current_season)
  
  prediction <- create_current_season_prediction(input_data_past_seasons$aggregated_football_data, 
                                                 input_data_this_season,
                                                 all_models)
  
  next_game_round_prediction <- predict_next_game_round(prediction, local_input$data_source_info, settings)
  
  all_results_tables <- do_monte_carlo_simulation(prediction, football_data_new, namen, settings, run_number)
  output <- list(all_results_tables = all_results_tables,
                 prediction = prediction,
                 next_game_round_prediction = next_game_round_prediction)
  return(output)
}