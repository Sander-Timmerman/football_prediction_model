create_models_for_game_round <- function(model_input, old_model = NULL, fixed_model = NULL, threshold = 0.01) {
  model_input_punten <- model_input %>%
    select(-c(Competitie, Seizoen, Team, Doelsom_dit_seizoen))
  model_punten <- create_model_with_and_without_shots(model_input_punten, old_model$punten, fixed_model$punten, threshold)
  
  model_input_goals <- model_input %>%
    select(-c(Competitie, Seizoen, Team, Punten_dit_seizoen))
  model_goals <- create_model_with_and_without_shots(model_input_goals, old_model$goals, fixed_model$goals, threshold)
  
  return(list(punten = model_punten,
              goals = model_goals))
}