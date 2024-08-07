create_current_season_prediction <- function(aggregated_football_data_old, input_data_new, all_models, write_results, run_number) {
  flog.info("Using models to make a prediction for current season")
  aggregated_football_data_new <- input_data_new$aggregated_football_data
  aggregated_level_two_data <- input_data_new$aggregated_level_two_data
  aggregated_transfermarkt_data_new <- input_data_new$aggregated_transfermarkt_data

  aggregated_football_data_with_shots <- aggregated_football_data_old[!is.na(aggregated_football_data_old$Schotsaldo), ]
  aggregated_football_data_without_shots <- aggregated_football_data_old[is.na(aggregated_football_data_old$Schotsaldo), ] %>%
    mutate(Doelpuntenvoor = NA,
           Doelpuntentegen = NA) %>%
    select(Team, Competitie, Seizoen, Aantalwedstrijden, Punten, Doelpuntenvoor, Doelpuntentegen, Niveau, Doelsaldo, Goalratio, Doelsom)
  aggregated_level_two_data <- aggregated_level_two_data %>%
    rbind(aggregated_football_data_without_shots)
  
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
  
  if(write_results) {
    write.xlsx(prediction, file.path("output", run_number, paste0("ratings.xlsx")))
    flog.info(paste0("Written predicted future points and goals in the output folder"))
  }
  
  return(prediction)
}