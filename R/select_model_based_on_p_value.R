select_model_based_on_p_value <- function(model, threshold) {
  p_values <- summary(model)$coefficients[-1, 4]
  max_p_value <- max(p_values)
  while(max_p_value > threshold) {
    variable_to_remove <- names(p_values)[which.max(p_values)]
    old_model_variables <- model$model
    new_model_variables <- setdiff(names(old_model_variables), variable_to_remove)
    model <- lm(as.formula(paste(new_model_variables[1], "~", paste(new_model_variables[-1], collapse = " + "))), data = na.omit(old_model_variables))
    p_values <- summary(model)$coefficients[-1, 4]
    max_p_value <- max(p_values)
  }
  return(model)
}