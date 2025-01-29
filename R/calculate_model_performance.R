calculate_model_performance <- function(model, number_games, total_goals) {
  rse <- summary(model)$sigma
  
  if(colnames(model$model)[1] == "Punten_dit_seizoen") {
    draw_probability <- 0.28 - abs(1.36 - model$fitted.values) ^ 2 * 0.1
    win_probability <- (model$fitted.values - draw_probability) / 3
    
    e_x <- win_probability * 3 + draw_probability
    e_x_2 <- win_probability * 9 + draw_probability
    perfect_variance_per_game <- e_x_2 - e_x ^ 2
    fourth_central_moment <- 
      win_probability * (3 - e_x) ^ 4 +
      draw_probability * (1 - e_x) ^ 4 +
      (1 - win_probability - draw_probability) * (0 - e_x) ^ 4
    kurtosis <- fourth_central_moment / perfect_variance_per_game ^ 2
  } else {
    perfect_variance_per_game <- model$fitted.values / (total_goals + 0.25)
    kurtosis <- 3 + 1 / (model$fitted.values * total_goals)
  }

  perfect_variance <- mean(perfect_variance_per_game / number_games)
  variance <- rse ^ 2 - perfect_variance
  rse_adjusted <- sign(variance) * sqrt(abs(variance))
  variance_of_variance <- mean(perfect_variance_per_game / number_games *
    (kurtosis - 1 + (2 / (number_games - 1))) / 
    number_games)
  performance_sd <- sqrt(variance_of_variance)
  return(c(rse_adjusted, performance_sd))
}