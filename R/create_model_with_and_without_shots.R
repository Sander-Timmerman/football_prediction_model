create_model_with_and_without_shots <- function(model_input, old_model = NULL, fixed_model = NULL, threshold = 0.01) {
  if(!is.null(old_model)) {
    model_input_shots <- model_input[-(which(!(colnames(model_input) %in% 
                                           names(old_model$with_shots$coefficients)))[-1])]
  } else {
    model_input_shots <- model_input
  }
  
  non_removable_variables <- names(fixed_model$with_shots$coefficients)[-1]
  model_with_shots <- fit_model(model_input_shots, threshold, non_removable_variables)
  
  if(!is.null(old_model)) {
    model_input_without_shots <- model_input[-(which(!(colnames(model_input) %in% 
                                           names(old_model$without_shots$coefficients)))[-1])]
  } else {
    model_input_without_shots <- model_input
  }
  non_removable_variables <- names(fixed_model$without_shots$coefficients)[-1]

  shots_last_season <- which(substr(colnames(model_input_without_shots), 1, 5) == "Schot" & 
                               substr(colnames(model_input_without_shots), 
                                      nchar(colnames(model_input_without_shots)) - 13, 
                                      nchar(colnames(model_input_without_shots))) == "_vorig_seizoen")
  if(length(shots_last_season) > 0) {
    model_input_without_shots <- model_input_without_shots[-shots_last_season]
  } else {
    model_input_without_shots <- model_input_without_shots
  }
  model_without_shots <- fit_model(model_input_without_shots, threshold, non_removable_variables)
  
  return(list(with_shots = model_with_shots,
              without_shots = model_without_shots))
}