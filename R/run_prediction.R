run_prediction <- function(all_cache_numbers, local_input, settings, run_number) {
  
  input_data_past_seasons <- load_input_data(all_cache_numbers$football_data_cache, 
                                             all_cache_numbers$aggregated_football_data_cache, 
                                             all_cache_numbers$transfermarkt_data_cache, 
                                             all_cache_numbers$player_jsons_cache,
                                             all_cache_numbers$all_final_standings_cache,
                                             run_number,
                                             local_input,
                                             is_current_season = FALSE,
                                             settings$current_season)
  
  competition_parameters <- calculate_competition_parameters(input_data_past_seasons$football_data, 
                                                             input_data_past_seasons$aggregated_football_data,
                                                             settings$current_season)
  
  all_models <- use_function_with_caching(all_cache_numbers$all_models_cache, 
                                          "all_models",
                                          run_number,
                                          train_models,
                                          input_data_past_seasons, 
                                          local_input$names,
                                          all_cache_numbers$aggregated_football_data_cache,
                                          run_number,
                                          competition_parameters)
  
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
                                                 all_models,
                                                 settings$write_results,
                                                 run_number,
                                                 competition_parameters,
                                                 local_input$names)
  
  next_game_round_prediction <- predict_next_game_round(select(prediction, -c(Punten_sd, Goals_sd)), 
                                                        local_input$data_source_info, 
                                                        settings, 
                                                        run_number,
                                                        competition_parameters)
  
  all_results_tables <- do_monte_carlo_simulation(prediction, 
                                                  input_data_this_season$football_data, 
                                                  local_input$names, 
                                                  settings, 
                                                  run_number,
                                                  competition_parameters)
  
  output <- list(all_results_tables = all_results_tables,
                 prediction = prediction,
                 next_game_round_prediction = next_game_round_prediction)
  
  saveRDS(output, file = file.path("output", run_number, "output.rds"))
  flog.info(paste0("Saved output"))
  
  return(output)
}