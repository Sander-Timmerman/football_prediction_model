calculate_competition_parameters <- function(football_data, aggregated_football_data, current_season) {
  competition_parameters <- list()
  home_matches <- football_data[seq(1, nrow(football_data) - 1, by = 2), ]
  
  competition_parameters$home_advantage <- sum(home_matches$FTHG) / 
    sum(home_matches$FTHG, home_matches$FTAG) * 2
  
  goals_per_competition <- home_matches <- home_matches %>%
    group_by(Competitie, Seizoen) %>%
    summarise(Goals = mean(FTHG + FTAG))
  
  pred_goals_per_competition <- predict(lm(Goals ~ Competitie + Seizoen, data = home_matches), 
                                        newdata = data.frame(Competitie = unique(home_matches$Competitie), 
                                                             Seizoen = current_season))
  names(pred_goals_per_competition) <- unique(home_matches$Competitie)
  competition_parameters$goals_per_competition <- pred_goals_per_competition / 2
  
  competition_parameters$points_to_goalratio <- lm(Goalratio ~ Punten, data = aggregated_football_data)$coefficients

  return(competition_parameters)
}