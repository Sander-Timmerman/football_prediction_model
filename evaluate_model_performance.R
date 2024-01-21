performance <- NULL
for(i in seq_len(length(all_models))) {
  rse <- summary(all_models[[i]]$punten$without_shots)$sigma
  perfect_variance <- 1.68 / (37 - i)
  variance <- rse ^ 2 - perfect_variance
  rse_adjusted <- sign(variance) * sqrt(abs(variance))
  performance <- c(performance, rse_adjusted)
}
plot(performance)