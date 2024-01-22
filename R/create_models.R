create_models <- function(all_models_cache, aggregated_transfermarkt_data_cache, player_jsons_cache, football_data, aggregated_football_data) {
  if(is.null(all_models_cache)) {
    if(is.null(aggregated_transfermarkt_data_cache)) {
      flog.info("Starts gathering and aggregating data from Transfermarkt from previous seasons")
      df_startdatums <- football_data %>%
        filter(Niveau == 1) %>%
        select(Competitie, Seizoen, Startdatum) %>%
        distinct()
      urls_tm <- read.csv("input/transfermarkt.csv") %>%
        inner_join(df_startdatums, by = c("Competitie", "Seizoen"))
      
      if(is.null(player_jsons_cache)) {
        player_jsons <- list()
      } else {
        player_jsons <- load(player_jsons_cache)
      }
      transfermarkt_data <- gather_transfermarkt_data(urls_tm, player_jsons_cache, current_season = FALSE)
      
      aggregated_transfermarkt_data <- aggregate_transfermarkt_data(transfermarkt_data)
      save(aggregated_transfermarkt_data, file = paste0("cache/aggregated_transfermarkt_data_",
                                                        Sys.Date(),
                                                        ".RData"))
    } else {
      load(aggregated_transfermarkt_data_cache)
      flog.info("Loaded aggregated football data from Transfermarkt from cache")
    }
    
    all_models <- train_models(football_data, aggregated_football_data, aggregated_transfermarkt_data)
  } else {
    load(all_models_cache)
    flog.info("Loaded prediction models from cache")
  }
  return(all_models)
}