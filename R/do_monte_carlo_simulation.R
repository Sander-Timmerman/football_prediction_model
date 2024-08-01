do_monte_carlo_simulation <- function(prediction, football_data_new, namen, n_sims, write_results) {
  for (competition in unique(prediction$Competitie)) {
    flog.info(paste0("Starting Monte Carlo simulation for competition ", competition))
    played_matches <- football_data_new[seq(1, nrow(football_data_new), 2),] %>%
      filter(Competitie == competition) %>%
      select(HomeTeam, AwayTeam, FTHG, FTAG, HPts, APts) %>%
      mutate(HomeTeam = mgsub(as.character(HomeTeam), namen$Footballdata, namen$Transfermarkt),
             AwayTeam = mgsub(as.character(AwayTeam), namen$Footballdata, namen$Transfermarkt))
    current_standings <- played_matches %>%
      calculate_standings()
    
    prediction_competition <- prediction %>%
      filter(Competitie == competition) %>%
      arrange(Team)
    all_teams <- unique(prediction_competition$Team)
    n_teams <- length(all_teams)
    
    current_standings[18, "Punten"] <- current_standings[18, "Punten"] - 18
    
    matches_to_simulate <- expand.grid(HomeTeam = all_teams, AwayTeam = all_teams, stringsAsFactors = FALSE) %>%
      filter(HomeTeam != AwayTeam) %>%
      mutate(Match = paste(HomeTeam, AwayTeam)) %>%
      filter(!(Match %in% paste(played_matches$HomeTeam, played_matches$AwayTeam))) %>%
      select(-Match)
    
    all_simulations <- data.frame(Team = rep(all_teams, n_sims),
                                  SimNr = rep(1 : n_sims, each = n_teams),
                                  Punten = rep(NA, n_teams * n_sims),
                                  Doelpuntenvoor = rep(NA, n_teams * n_sims),
                                  Doelsaldo = rep(NA, n_teams * n_sims),
                                  Rank = rep(NA, n_teams * n_sims))
    
    pb <- winProgressBar(title = paste0("Simulating ", competition),
                         label = "Simulating has started",
                         min = 0,
                         max = n_sims,
                         initial = 0)
    
    for (sim_nr in 1 : n_sims) {
      flog.debug(paste0("Starting simulation number ", sim_nr))
      total_standings <- run_simulation(prediction_competition, matches_to_simulate, current_standings)    
      all_simulations[(n_teams * (sim_nr - 1) + 1) : (n_teams * sim_nr), 3 : 6] <- total_standings
      
      info <- sprintf("Simulating %d%% completed", round((sim_nr / n_sims * 100)))
      setWinProgressBar(pb, sim_nr, label = info)
    }
    close(pb)
    
    results_table <- create_results_table(all_simulations, n_sims, prediction_competition)
    if(write_results) {
      write.xlsx(results_table, paste0("output/results_table_", competition, "_on_", Sys.Date(), ".xlsx"))
      flog.info(paste0("Written results table for competition ", competition, " in the output folder"))
    }
  }
  return(results_table)
}