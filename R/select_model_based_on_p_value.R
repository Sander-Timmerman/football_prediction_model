select_model_based_on_p_value <- function(model, threshold, non_removable_variables = NULL) {
  p_values <- summary(model)$coefficients[-1, 4]
  max_p_value <- max(p_values)
  while(max_p_value > threshold) {
    variable_to_remove <- names(p_values)[which.max(p_values)]
    old_model_data <- model$model
    old_model_variables <- names(old_model_data)
    if(old_model_variables[1] == "Punten_dit_seizoen" & 
       sum(substr(old_model_variables, 
                  nchar(old_model_variables) - 12, 
                  nchar(old_model_variables)) == "vorig_seizoen") > 1) {
      non_removable_variables <- c(non_removable_variables, "Niveau_vorig_seizoen")
    }
    if(variable_to_remove %in% non_removable_variables) {
      p_values_removable <- p_values[!(old_model_variables[-1] %in% non_removable_variables)]
      if(max(p_values_removable) > threshold) {
        variable_to_remove <- names(p_values_removable)[which.max(p_values_removable)]
      } else break
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