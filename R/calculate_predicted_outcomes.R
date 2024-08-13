calculate_predicted_outcomes <- function(football_data) {
  pred_points <- football_data %>%
    group_by(HomeTeam, Seizoen) %>%
    summarise(Points = mean(HomePoints),
              Goals = mean(Goals)) %>%
    ungroup() %>%
    rename(Team = HomeTeam)
  
  return(pred_points)
}