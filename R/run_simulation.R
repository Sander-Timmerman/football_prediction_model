run_simulation <- function(prediction_competition, matches_to_simulate, current_standings, competition_parameters) {
  prediction_simulation <- prediction_competition %>%
    mutate(Punten = rnorm(nrow(prediction_competition), mean = Punten, sd = max(0, Punten_sd)),
           Goals = rnorm(nrow(prediction_competition), mean = Goals, sd = max(0, Goals_sd))) %>%
    select(-Punten_sd, -Goals_sd) %>%
    calculate_goal_expectations(competition_parameters$points_to_goalratio)
  
  matches_to_simulate <- calculate_match_expectations(matches_to_simulate, 
                                                      prediction_simulation, 
                                                      competition_parameters$goals - 0.15,
                                                      competition_parameters$home_advantage)
  
  simulation_standings <- matches_to_simulate %>%
    mutate(FTHG = rpois(nrow(matches_to_simulate), lambda = matches_to_simulate$ExpHG),
           FTAG = rpois(nrow(matches_to_simulate), lambda = matches_to_simulate$ExpAG),
           HPts = 3 * (FTHG > FTAG) + (FTHG == FTAG),
           APts = 3 * (FTAG > FTHG) + (FTHG == FTAG)) %>%
    calculate_standings()
  
  if(nrow(current_standings) == nrow(simulation_standings)) {
    total_standings <- (current_standings[-1] + simulation_standings[-1])
  } else {
    total_standings <- combine_standings(current_standings, simulation_standings)[-1]
  }
  total_standings <- total_standings %>%
    mutate(Rank = rank(-(Punten + Doelsaldo * 0.01 + Doelpuntenvoor * 0.0001), ties.method = "random"))
  return(total_standings)
}