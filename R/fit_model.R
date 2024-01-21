fit_model <- function(model_input) {
  model <- lm(formula(model_input), data = na.omit(model_input))
  model <- MASS::stepAIC(model, direction = "both")
  return(model)
}