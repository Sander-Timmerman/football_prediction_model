use_model_on_new_data <- function(aggregated_football_data, aggregated_transfermarkt_data_new, aggregated_football_data_new, all_models, with_shots) {
  shots <- if(with_shots) "with_shots" else "without_shots"

  model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data_new, is_new_data = TRUE)
  model_input <- add_in_season_info(model_input, aggregated_football_data_new)
  
  prediction <- data.frame()
  for(game_round in unique(model_input$Aantalwedstrijden)) {
    model_input_filtered <- filter(model_input, Aantalwedstrijden == game_round)
    model_input_filtered <- mutate(model_input_filtered,
                                   Punten = predict(all_models[[min(25, game_round + 1)]]$punten[[shots]], newdata = model_input_filtered),
                                   Goals = predict(all_models[[min(25, game_round + 1)]]$goals[[shots]], newdata = model_input_filtered)) %>%
      select(Team, Competitie, Punten, Goals)
    prediction <- rbind(prediction, model_input_filtered)
  }
  return(prediction)
}