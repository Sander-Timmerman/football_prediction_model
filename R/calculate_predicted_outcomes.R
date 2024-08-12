calculate_predicted_outcomes <- function(football_data) {
  pred_points <- football_data %>%
    group_by(HomeTeam, Seizoen) %>%
    summarise(Points = mean(HomePoints)) %>%
    ungroup()
  
  return(pred_points$Points)
}