select_model_based_on_p_value <- function(model, threshold) {
  p_values <- summary(model)$coefficients[-1, 4]
  max_p_value <- max(p_values)
  while(max_p_value > threshold) {
    variable_to_remove <- names(p_values)[which.max(p_values)]
    old_model_data <- model$model
    old_model_variables <- names(old_model_data)
    if(variable_to_remove == "Niveau_vorig_seizoen" & old_model_variables[1] == "Punten_dit_seizoen") {
      if(sum(substr(old_model_variables, 
                    nchar(old_model_variables) - 12, 
                    nchar(old_model_variables)) == "vorig_seizoen") > 1) {
        p_values_without_max <- p_values[-which.max(p_values)]
        if(max(p_values_without_max) > threshold) {
          variable_to_remove <- names(p_values_without_max)[which.max(p_values_without_max)]
        } else break
      }
    }
    new_model_variables <- setdiff(old_model_variables, variable_to_remove)
    model <- lm(as.formula(paste(new_model_variables[1], 
                                 "~", 
                                 paste(new_model_variables[-1], collapse = " + "))), 
                data = na.omit(old_model_data))
    p_values <- summary(model)$coefficients[-1, 4]
    max_p_value <- max(p_values)
  }
  return(model)
}