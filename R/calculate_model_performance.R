calculate_model_performance <- function(model, number_games) {
  rse <- summary(model)$sigma
  
  draw_probability <- 0.28 - abs(1.36 - model$fitted.values) ^ 2 * 0.1
  win_probability <- (model$fitted.values - draw_probability) / 3
  
  perfect_variance_per_game <- 9 * win_probability * (1 - win_probability) + 
    draw_probability * (1 - draw_probability) - 
    6 * win_probability * draw_probability
  
  perfect_variance <- sum(perfect_variance_per_game / number_games) / length(number_games)
  variance <- rse ^ 2 - perfect_variance
  rse_adjusted <- sign(variance) * sqrt(abs(variance))
  
  kurtosis <- (81 * win_probability + draw_probability) / perfect_variance_per_game ^ 2
  variance_of_variance <- 2 * 
    sum(perfect_variance_per_game ^ 2 / number_games * (1 + (kurtosis - 3) / number_games)) / 
    length(number_games) ^ 2
  performance_sd <- sqrt(variance_of_variance)
  return(c(rse_adjusted, performance_sd))
}