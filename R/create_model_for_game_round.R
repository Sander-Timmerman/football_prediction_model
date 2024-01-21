create_models_for_game_round <- function(model_input) {
  model_input_punten <- model_input %>%
    select(-c(Competitie, Seizoen, Team, Doelsom_dit_seizoen))
  model_punten <- create_model_with_and_without_shots(model_input_punten)
  
  model_input_goals <- model_input %>%
    select(-c(Competitie, Seizoen, Team, Punten_dit_seizoen))
  model_goals <- create_model_with_and_without_shots(model_input_goals)
  
  return(list(punten = model_punten,
              goals = model_goals))
}