create_current_season_prediction <- function(aggregated_football_data, aggregated_transfermarkt_data_new, aggregated_football_data_new, aggregated_level_two_data, all_models) {
  flog.info("Using models to make a prediction for current season")
  
  aggregated_football_data_with_shots <- aggregated_football_data[!is.na(aggregated_football_data$Schotsaldo), ]
  aggregated_football_data_without_shots <- aggregated_football_data[is.na(aggregated_football_data$Schotsaldo), ]
  browser()
  aggregated_level_two_data <- left_join(aggregated_level_two_data, aggregated_football_data_without_shots, by = c("Team", "Competitie", "Seizoen"))
  
  prediction_with_shots <- use_model_on_new_data(aggregated_football_data_with_shots, 
                                                 aggregated_transfermarkt_data_new, 
                                                 aggregated_football_data_new,
                                                 all_models,
                                                 with_shots = TRUE)
  prediction_without_shots <- use_model_on_new_data(aggregated_level_two_data, 
                                                    aggregated_transfermarkt_data_new, 
                                                    aggregated_football_data_new,
                                                    all_models,
                                                    with_shots = FALSE)
  prediction <- rbind(prediction_with_shots, prediction_without_shots)
  return(prediction)
}