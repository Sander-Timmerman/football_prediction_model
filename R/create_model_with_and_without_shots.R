create_model_with_and_without_shots <- function(model_input) {
  model_with_shots <- fit_model(model_input)

  shots_last_season <- which(substr(colnames(model_input), 1, 5) == "Schot" & 
                               substr(colnames(model_input), 
                                      nchar(colnames(model_input)) - 13, 
                                      nchar(colnames(model_input))) == "_vorig_seizoen")
  if(length(shots_last_season) > 0) {
    model_input_without_shots <- model_input[-shots_last_season]
  } else {
    model_input_without_shots <- model_input
  }
  model_without_shots <- fit_model(model_input_without_shots)
  
  return(list(with_shots = model_with_shots,
              without_shots = model_without_shots))
}