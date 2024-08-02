create_models <- function(aggregated_transfermarkt_data_cache, transfermarkt_data_cache, player_jsons_cache, football_data, aggregated_football_data, data_source_info, is_current_season, namen) {
  if(is.null(aggregated_transfermarkt_data_cache)) {
    flog.info("Starts gathering and aggregating data from Transfermarkt from previous seasons")
    
    if(is.null(player_jsons_cache)) {
      player_jsons <- list()
    } else {
      load(player_jsons_cache)
      flog.info("Loaded player_jsons from cache")
    }
    if(is.null(transfermarkt_data_cache)) {
      df_startdatums <- football_data %>%
        filter(Niveau == 1) %>%
        select(Competitie, Seizoen, Startdatum) %>%
        distinct()
      urls_tm <- find_data_urls(data_source_info, "transfermarkt", is_current_season, 25, "startseite", 1) %>%
        inner_join(df_startdatums, by = c("Competitie", "Seizoen"))
      transfermarkt_data <- gather_transfermarkt_data(urls_tm, player_jsons, is_current_season)
    } else {
      load(transfermarkt_data_cache)
      flog.info("Loaded transfermarkt_data from cache")
    }
    
    aggregated_transfermarkt_data <- aggregate_transfermarkt_data(transfermarkt_data)
    save(aggregated_transfermarkt_data, file = paste0("cache/aggregated_transfermarkt_data_",
                                                      Sys.Date(),
                                                      ".RData"))
  } else {
    load(aggregated_transfermarkt_data_cache)
    flog.info("Loaded aggregated football data from Transfermarkt from cache")
  }
  
  all_models <- train_models(football_data, aggregated_football_data, aggregated_transfermarkt_data, namen)
  return(all_models)
}