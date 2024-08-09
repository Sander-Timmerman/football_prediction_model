calculate_goal_expectations <- function(prediction, points_to_goalratio) {
  prediction <- prediction %>%
    mutate(Doelpuntenvoor = points_to_goalratio[2] * Punten * Goals + points_to_goalratio[1] * Goals,
           Doelpuntentegen = Goals - Doelpuntenvoor) %>%
    select(-(Punten : Goals), -Competitie)
  return(prediction)
}