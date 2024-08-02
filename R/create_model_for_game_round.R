create_models_for_game_round <- function(model_input, old_model = NULL) {
  model_input_punten <- model_input %>%
    select(-c(Competitie, Seizoen, Team, Doelsom_dit_seizoen))
  if(!is.null(old_model)) {
    # model_input_punten <- model_input_punten[-(which(!(colnames(model_input_punten)[2 : 13] %in% 
    #                                                      names(old_model$punten$with_shots$coefficients))) + 1)]
  }
  model_punten <- create_model_with_and_without_shots(model_input_punten)
  
  model_input_goals <- model_input %>%
    select(-c(Competitie, Seizoen, Team, Punten_dit_seizoen))
  if(!is.null(old_model)) {
    # model_input_goals <- model_input_goals[-(which(!(colnames(model_input_goals)[2 : 13] %in% 
    #                                                    names(old_model$goals$with_shots$coefficients))) + 1)]
  }
  model_goals <- create_model_with_and_without_shots(model_input_goals)
  
  return(list(punten = model_punten,
              goals = model_goals))
}