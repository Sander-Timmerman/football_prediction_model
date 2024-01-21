create_model_with_and_without_shots <- function(model_input) {
  model_with_shots <- fit_model(model_input)
  
  model_input_without_shots <- model_input %>%
    select(-c(Schotenvoor_vorig_seizoen : Schotenopdoelsom_vorig_seizoen))
  model_without_shots <- fit_model(model_input_without_shots)
  
  return(list(with_shots = model_with_shots,
              without_shots = model_without_shots))
}