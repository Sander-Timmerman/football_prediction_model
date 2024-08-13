create_results_table <- function(all_simulations, n_sims, prediction_competition) {
  results_table <- as.data.frame.matrix(table(as.character(all_simulations$Team), all_simulations$Rank)) / n_sims
  results_table <- all_simulations %>%
    group_by(Team) %>%
    summarise(Positie = mean(Rank),
              Punten = mean(Punten)) %>%
    mutate(Rating = prediction_competition$Punten) %>%
    cbind(results_table) %>%
    arrange(Positie)
  rownames(results_table) <- NULL
  
  return(results_table)
}