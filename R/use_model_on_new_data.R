use_model_on_new_data <- function(aggregated_football_data, aggregated_transfermarkt_data_new, aggregated_football_data_new, all_models, with_shots) {
  shots <- if(with_shots) "with_shots" else "without_shots"
  
  model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data_new, is_new_data = TRUE)
  model_input <- add_in_season_info(model_input, aggregated_football_data_new)
  
  prediction <- data.frame()
  for(game_round in unique(model_input$Aantalwedstrijden)) {
    model_input_filtered <- filter(model_input, Aantalwedstrijden == game_round)
    
    model_punten <- all_models[[min(23, game_round + 1)]]$punten[[shots]]
    model_goals <- all_models[[min(22, game_round + 1)]]$goals[[shots]]
    
    model_input_filtered <- mutate(model_input_filtered,
                                   Punten = predict(model_punten, newdata = model_input_filtered),
                                   Punten_sd = model_punten$performance,
                                   Goals = predict(model_goals, newdata = model_input_filtered),
                                   Goals_sd = model_goals$performance) %>%
      select(Team, Competitie, Punten, Punten_sd, Goals, Goals_sd)
    prediction <- rbind(prediction, model_input_filtered)
  }
  return(prediction)
}