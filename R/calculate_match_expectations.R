calculate_match_expectations <- function(matches_to_simulate, goal_expectations, goals_per_match, home_advantage = 1.1375) {
  matches_to_simulate <- matches_to_simulate %>%
    inner_join(goal_expectations, by=c("HomeTeam"="Team")) %>%
    inner_join(goal_expectations, by=c("AwayTeam"="Team")) %>%
    mutate(Home_advantage = home_advantage,
           ExpHG = Doelpuntenvoor.x * Doelpuntentegen.y * Home_advantage * goals_per_match,
           ExpAG = Doelpuntenvoor.y * Doelpuntentegen.x * (2 - Home_advantage) * goals_per_match) %>%
    ungroup() %>%
    select(-(Doelpuntenvoor.x : Home_advantage))
  
  n_matches <- nrow(matches_to_simulate)
  all_probs <- NULL
  for(row in seq_len(n_matches)) {
    goal_prob_mat <- outer(dpois(0:10, matches_to_simulate[row, "ExpHG"]), 
                           dpois(0:10, matches_to_simulate[row, "ExpAG"]))
    
    home_prob <- sum(goal_prob_mat[lower.tri(goal_prob_mat)]) - 
      0.05 * sum(goal_prob_mat[row(goal_prob_mat) == col(goal_prob_mat) + 1])
    away_prob <- sum(goal_prob_mat[upper.tri(goal_prob_mat)]) - 
      0.05 * sum(goal_prob_mat[row(goal_prob_mat) + 1 == col(goal_prob_mat)])
    draw_prob <- sum(diag(goal_prob_mat)) + 
      0.05 * sum(goal_prob_mat[row(goal_prob_mat) == col(goal_prob_mat) + 1]) + 
      0.05 * sum(goal_prob_mat[row(goal_prob_mat) + 1 == col(goal_prob_mat)])
    all_probs <- c(all_probs, home_prob, draw_prob, away_prob)
  }
  
  matches_to_simulate <- mutate(matches_to_simulate,
                                Home_prob = all_probs[seq(1, n_matches * 3, by = 3)],
                                Draw_prob = all_probs[seq(2, n_matches * 3, by = 3)],
                                Away_prob = all_probs[seq(3, n_matches * 3, by = 3)])
  
  return(matches_to_simulate)
}