run_prediction <- function(aggregated_football_data_cache = NULL, 
                           all_models_cache = NULL, 
                           player_jsons_cache = NULL,
                           transfermarkt_data_cache = NULL,
                           aggregated_transfermarkt_data_cache = NULL, 
                           aggregated_transfermarkt_data_new_cache = NULL,
                           all_final_standings_cache = NULL,
                           n_sims = 10000,
                           write_results = TRUE) {
  data_source_info <- read.csv("input/data_source_info.csv", stringsAsFactors = FALSE)
  namen <- read.csv("input/all_club_names.csv")
  is_current_season <- FALSE
  
  if(is.null(aggregated_football_data_cache) | is.null(all_models_cache)) {
    flog.info("Starts gathering and aggregating data from football_data from previous seasons")
    if(!(is.null(aggregated_football_data_cache))) {
      flog.warn("aggregated_football_data_cache is ignored because all_models_cache is empty")
    }
    
    urls_fd <- find_data_urls(data_source_info, "football_data", is_current_season, 25)
    football_data <- gather_football_data(urls_fd, namen)
    aggregated_football_data <- aggregate_football_data(football_data, namen)
    save(aggregated_football_data, file = paste0("cache/aggregated_football_data_",
                                                 Sys.Date(),
                                                 ".RData"))
  } else {
    load(aggregated_football_data_cache)
    flog.info("Loaded aggregated data from football_data from previous seasons from cache")
  }
  
  all_models <- create_models(all_models_cache, aggregated_transfermarkt_data_cache, transfermarkt_data_cache, player_jsons_cache, football_data, aggregated_football_data, data_source_info, is_current_season, namen)
    
  is_current_season <- TRUE
  
  if(is.null(aggregated_transfermarkt_data_new_cache)) {
    flog.info("Starts gathering and aggregating data from Transfermarkt from current season")
    urls_tm_new <- find_data_urls(data_source_info, "transfermarkt", is_current_season, 25)
    urls_tm_new$Startdatum <- Sys.Date()
    transfermarkt_data_new <- gather_transfermarkt_data(urls_tm_new, is_current_season = is_current_season)
    aggregated_transfermarkt_data_new <- aggregate_transfermarkt_data(transfermarkt_data_new)
    
    save(aggregated_transfermarkt_data_new, file = paste0("cache/aggregated_transfermarkt_data_new_",
                                                          Sys.Date(),
                                                          ".RData"))
  } else {
    load(aggregated_transfermarkt_data_new_cache)
    flog.info("Loaded aggregated Transfermarkt data from current season from cache")
  }
  
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