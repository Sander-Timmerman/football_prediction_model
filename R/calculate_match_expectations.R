calculate_match_expectations <- function(matches_to_simulate, goal_expectations, goals_per_match, home_advantage = 1.1375) {
  matches_to_simulate <- matches_to_simulate %>%
    inner_join(goal_expectations, by=c("HomeTeam"="Team")) %>%
    inner_join(goal_expectations, by=c("AwayTeam"="Team")) %>%
    mutate(Home_advantage = home_advantage,
           ExpHG = Doelpuntenvoor.x * Doelpuntentegen.y * Home_advantage * goals_per_match,
           ExpAG = Doelpuntenvoor.y * Doelpuntentegen.x * (2 - Home_advantage) * goals_per_match) %>%
    ungroup() %>%
    select(-(Doelpuntenvoor.x : Home_advantage))
  
  return(matches_to_simulate)
}