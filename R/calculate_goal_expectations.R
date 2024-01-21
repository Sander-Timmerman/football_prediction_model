calculate_goal_expectations <- function(prediction) {
  prediction <- prediction %>%
    mutate(Doelpuntenvoor = 0.266252 * Punten * Goals + 0.128078 * Goals,
           Doelpuntentegen = Goals - Doelpuntenvoor) %>%
    select(-(Punten : Goals), -Competitie)
  return(prediction)
}