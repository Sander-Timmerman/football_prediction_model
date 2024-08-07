load_cache_numbers <- function(football_data_cache,
                               aggregated_football_data_cache, 
                               all_models_cache, 
                               player_jsons_cache,
                               transfermarkt_data_cache,
                               transfermarkt_data_new_cache,
                               all_final_standings_cache) {
  if(is.null(all_models_cache) & !(is.null(aggregated_football_data_cache))) {
    if(is.null(football_data_cache)) {
      aggregated_football_data_cache <- NULL
      flog.warn("Aggregated_football_data_cache will not be used")
    } else if(football_data_cache != aggregated_football_data_cache) {
      aggregated_football_data_cache <- NULL
      flog.warn("Aggregated_football_data_cache will not be used")
    }
   }
  
  all_cache_paths <- list()
  all_cache_paths$football_data_cache <- football_data_cache
  all_cache_paths$aggregated_football_data_cache <- aggregated_football_data_cache
  all_cache_paths$all_models_cache <- all_models_cache
  all_cache_paths$player_jsons_cache <- player_jsons_cache
  all_cache_paths$transfermarkt_data_cache <- transfermarkt_data_cache
  all_cache_paths$transfermarkt_data_new_cache <- transfermarkt_data_new_cache
  all_cache_paths$all_final_standings_cache <- all_final_standings_cache
  
  return(all_cache_paths)
}