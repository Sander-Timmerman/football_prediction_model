calculate_model_performance <- function(model, mean_games, perfect_variance_one_game) {
  rse <- summary(model)$sigma
  perfect_variance <- perfect_variance_one_game / mean_games
  variance <- rse ^ 2 - perfect_variance
  rse_adjusted <- sign(variance) * sqrt(abs(variance))
  return(rse_adjusted)
}