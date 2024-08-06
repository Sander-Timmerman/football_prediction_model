do_monte_carlo_simulation <- function(prediction, football_data_new, namen, settings, run_number) {
  all_results_tables <- list()
  flog.info("Starting Monte Carlo simulations")
  for(competition in unique(prediction$Competitie)) {
    flog.info(paste0("Starting Monte Carlo simulation for competition ", competition))
    results_table <- tryCatch(
      {
        prediction_competition <- prediction %>%
          filter(Competitie == competition) %>%
          arrange(Team)
        all_teams <- unique(prediction_competition$Team)
        n_teams <- length(all_teams)
        
        played_matches <- football_data_new[seq(1, nrow(football_data_new), 2),] %>%
          filter(Competitie == competition) %>%
          select(HomeTeam, AwayTeam, FTHG, FTAG, HPts, APts) %>%
          mutate(HomeTeam = mgsub(as.character(HomeTeam), namen$Football_data, namen$Transfermarkt),
                 AwayTeam = mgsub(as.character(AwayTeam), namen$Football_data, namen$Transfermarkt))
        
        current_standings <- calculate_standings(played_matches) %>%
          right_join(data.frame(Team = all_teams))
        current_standings[is.na(current_standings)] <- 0
        
        matches_to_simulate <- expand.grid(HomeTeam = all_teams, AwayTeam = all_teams, stringsAsFactors = FALSE) %>%
          filter(HomeTeam != AwayTeam) %>%
          mutate(Match = paste(HomeTeam, AwayTeam)) %>%
          filter(!(Match %in% paste(played_matches$HomeTeam, played_matches$AwayTeam))) %>%
          select(-Match)
        
        all_simulations <- data.frame(Team = rep(all_teams, settings$n_sims),
                                      SimNr = rep(1 : settings$n_sims, each = n_teams),
                                      Punten = rep(NA, n_teams * settings$n_sims),
                                      Doelpuntenvoor = rep(NA, n_teams * settings$n_sims),
                                      Doelsaldo = rep(NA, n_teams * settings$n_sims),
                                      Rank = rep(NA, n_teams * settings$n_sims))
        
        pb <- winProgressBar(title = paste0("Simulating ", competition),
                             label = "Simulating has started",
                             min = 0,
                             max = settings$n_sims,
                             initial = 0)
        
        for (sim_nr in 1 : settings$n_sims) {
          flog.debug(paste0("Starting simulation number ", sim_nr))
          total_standings <- run_simulation(prediction_competition, matches_to_simulate, current_standings)    
          all_simulations[(n_teams * (sim_nr - 1) + 1) : (n_teams * sim_nr), 3 : 6] <- total_standings
          
          info <- sprintf("Simulating %d%% completed", round((sim_nr / settings$n_sims * 100)))
          setWinProgressBar(pb, sim_nr, label = info)
        }
        close(pb)
        
        results_table <- create_results_table(all_simulations, settings$n_sims, prediction_competition)
        if(settings$write_results) {
          write.xlsx(results_table, file.path("output", run_number, paste0("results_table_", competition, ".xlsx")))
          flog.info(paste0("Written results table for competition ", competition, " in the output folder"))
        }
        return(results_table)
      },
      error = function(e) {
        flog.error(paste0("Monte Carlo simulation for competition ", competition, " failed. Returning empty dataframe. Error message: ", e))
        return(data.frame())
      }
    )
    all_results_tables[[competition]] <- results_table
  }
  flog.info("Finished Monte Carlo simulation for all competitions")
  return(all_results_tables)
}