calculate_match_expectations <- function(matches_to_simulate, goal_expectations, goals_per_match) {
  matches_to_simulate <- matches_to_simulate %>%
    inner_join(goal_expectations, by=c("HomeTeam"="Team")) %>%
    inner_join(goal_expectations, by=c("AwayTeam"="Team")) %>%
    mutate(ExpHG = Doelpuntenvoor.x * Doelpuntentegen.y * 1.1375 * goals_per_match,
           ExpAG = Doelpuntenvoor.y * Doelpuntentegen.x * 0.8625 * goals_per_match) %>%
    ungroup() %>%
    select(-(Doelpuntenvoor.x : Doelpuntentegen.y))
  
  return(matches_to_simulate)
}