fit_model <- function(model_input, threshold = 0.01, non_removable_variables = NULL) {
  model <- lm(formula(model_input), data = na.omit(model_input))
  # model <- MASS::stepAIC(model, direction = "both")
  model <- select_model_based_on_p_value(model, threshold, non_removable_variables)
  return(model)
}