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
  
  is_current_season <- FALSE
  
  urls_fd <- find_data_urls(data_source_info, "football_data", is_current_season, 25)
  football_data <- use_function_with_caching(football_data_cache, 
                                             "football_data", 
                                             gather_football_data, 
                                             urls_fd, 
                                             namen)
  aggregated_football_data <- use_function_with_caching(aggregated_football_data_cache, 
                                                        "aggregated_football_data", 
                                                        aggregate_football_data, 
                                                        football_data, 
                                                        namen)
  all_models <- use_function_with_caching(all_models_cache, 
                                          "all_models", 
                                          create_models,
                                          transfermarkt_data_cache, 
                                          player_jsons_cache, 
                                          football_data, 
                                          aggregated_football_data, 
                                          data_source_info, 
                                          is_current_season, 
                                          namen)
    
  is_current_season <- TRUE
  
  flog.info("Starts gathering and aggregating data from Transfermarkt from current season")
  urls_tm_new <- find_data_urls(data_source_info, "transfermarkt", is_current_season, 25)
  urls_tm_new$Startdatum <- Sys.Date()
  transfermarkt_data_new <- use_function_with_caching(transfermarkt_data_new_cache,
                                                      "transfermarkt_data_new",
                                                      gather_transfermarkt_data,
                                                      urls_tm_new,
                                                      is_current_season = is_current_season)
  aggregated_transfermarkt_data_new <- aggregate_transfermarkt_data(transfermarkt_data_new)
  
  flog.info("Starts gathering and aggregating data from football_data from current season")
  urls_fd_new <- find_data_urls(data_source_info, "football_data", is_current_season, 25)
  football_data_new <- gather_football_data(urls_fd_new, namen)
  aggregated_football_data_new <- aggregate_football_data(football_data_new, namen)
  aggregated_level_two_data <- aggregate_level_two_final_standings(all_final_standings_cache, data_source_info, is_current_season)
  prediction <- create_current_season_prediction(aggregated_football_data, aggregated_transfermarkt_data_new, aggregated_football_data_new, aggregated_level_two_data, all_models)
  
  next_game_round_prediction <- predict_next_game_round(prediction)
  
  results_table <- do_monte_carlo_simulation(prediction, football_data_new, namen, n_sims, write_results)
  output <- list(results_table = results_table,
                 prediction = prediction,
                 next_game_round_prediction = next_game_round_prediction)
  return(output)
}