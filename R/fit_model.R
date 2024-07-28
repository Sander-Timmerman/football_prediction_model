fit_model <- function(model_input) {
  model <- lm(formula(model_input), data = na.omit(model_input))
  # model <- MASS::stepAIC(model, direction = "both")
  model <- select_model_based_on_p_value(model, 0.01)
  return(model)
}