run_prediction <- function(football_data_cache = NULL,
                           aggregated_football_data_cache = NULL, 
                           all_models_cache = NULL, 
                           player_jsons_cache = NULL,
                           transfermarkt_data_cache = NULL,
                           transfermarkt_data_new_cache = NULL,
                           all_final_standings_cache = NULL,
                           n_sims = 10000,
                           write_results = TRUE) {
  data_source_info <- read.csv("input/data_source_info.csv", stringsAsFactors = FALSE)
  namen <- read.csv("input/all_club_names.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if(is.null(all_models_cache) & !(is.null(aggregated_football_data_cache))) {
    aggregated_football_data_cache <- NULL
    flog.warn("Because all_models_cache is empty, aggregated_football_data_cache will not be used")
  }
  
  input_data_past_seasons <- load_input_data(football_data_cache, 
                                             aggregated_football_data_cache, 
                                             transfermarkt_data_cache, 
                                             player_jsons_cache,
                                             all_final_standings_cache,
                                             data_source_info, 
                                             namen, 
                                             is_current_season = FALSE)
  
  all_models <- use_function_with_caching(all_models_cache, 
                                          "all_models", 
                                          train_models,
                                          input_data_past_seasons, 
                                          namen)
  
  input_data_this_season <- load_input_data(football_data_cache = NULL, 
                                            aggregated_football_data_cache = NULL, 
                                            transfermarkt_data_cache = NULL, 
                                            player_jsons_cache = NULL,
                                            all_final_standings_cache,
                                            data_source_info, 
                                            namen, 
                                            is_current_season = TRUE)
  
  prediction <- create_current_season_prediction(input_data_past_seasons$aggregated_football_data, 
                                                 input_data_this_season,
                                                 all_models)
  
  next_game_round_prediction <- predict_next_game_round(prediction)
  
  results_table <- do_monte_carlo_simulation(prediction, football_data_new, namen, n_sims, write_results)
  output <- list(results_table = results_table,
                 prediction = prediction,
                 next_game_round_prediction = next_game_round_prediction)
  return(output)
}