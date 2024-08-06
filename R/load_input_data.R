load_input_data <- function(football_data_cache, aggregated_football_data_cache, transfermarkt_data_cache, player_jsons_cache, all_final_standings_cache, run_number, local_input, is_current_season, current_season) {
  flog.info(paste0("Starts loading data from ", if(is_current_season) "this season" else "past seasons"))
  input_data <- list()
  
  period_used <- if(is_current_season) "this_season" else "past_seasons"
  
  urls_fd <- find_data_urls(local_input$data_source_info, "football_data", is_current_season, current_season)
  football_data <- use_function_with_caching(football_data_cache, 
                                             paste0("football_data_", period_used),
                                             run_number,
                                             gather_football_data, 
                                             urls_fd, 
                                             local_input$names)
  aggregated_football_data <- use_function_with_caching(aggregated_football_data_cache, 
                                                        paste0("aggregated_football_data_", period_used),
                                                        run_number,
                                                        aggregate_football_data, 
                                                        football_data, 
                                                        local_input$names)
  input_data$football_data <- football_data
  input_data$aggregated_football_data <- aggregated_football_data
  
  if(is_current_season) {
    aggregated_level_two_data <- aggregate_level_two_final_standings(all_final_standings_cache, local_input$data_source_info, is_current_season, run_number)
    input_data$aggregated_level_two_data <- aggregated_level_two_data
  }
  
  player_jsons <- if(is_current_season) list() else use_function_with_caching(player_jsons_cache, 
                                                                              "player_jsons", 
                                                                              run_number, 
                                                                              list)
  df_startdatums <- football_data %>%
    filter(Niveau == 1) %>%
    select(Competitie, Seizoen, Startdatum) %>%
    distinct()
  urls_tm <- find_data_urls(local_input$data_source_info, "transfermarkt", is_current_season, current_season, "startseite", 1) %>%
    left_join(df_startdatums, by = c("Competitie", "Seizoen")) %>%
    mutate(Startdatum = if(is_current_season) Sys.Date() else Startdatum)
  transfermarkt_data <- use_function_with_caching(transfermarkt_data_cache,
                                                  paste0("transfermarkt_data_", period_used),
                                                  run_number,
                                                  gather_transfermarkt_data,
                                                  urls_tm,
                                                  player_jsons,
                                                  is_current_season,
                                                  local_input$months,
                                                  run_number)
  aggregated_transfermarkt_data <- aggregate_transfermarkt_data(transfermarkt_data)
  input_data$aggregated_transfermarkt_data <- aggregated_transfermarkt_data
  return(input_data)
}