run_prediction <- function(aggregated_football_data_cache = NULL, 
                           all_models_cache = NULL, 
                           player_jsons_cache = NULL, 
                           aggregated_transfermarkt_data_cache = NULL, 
                           aggregated_transfermarkt_data_new_cache = NULL) {
  
  if(is.null(aggregated_football_data_cache)) {
    urls_fd_first_level <- read.csv("input/footballdata2.csv")
    urls_fd_second_level <- read.csv("input/footballdataniveautwee.csv")
    
    football_data_first_level <- gather_football_data(urls_fd_first_level, 1)
    football_data_second_level <- gather_football_data(urls_fd_second_level, 2)
    football_data <- rbind(football_data_first_level, football_data_second_level)
    
    aggregated_football_data <- aggregate_football_data(football_data)
    save(aggregated_football_data, file = paste0("cache/aggregated_football_data_",
                                                 Sys.Date(),
                                                 ".RData"))
  } else {
    load(aggregated_football_data_cache)
  } 
  
  if(is.null(all_models_cache)) {
    if(is.null(aggregated_transfermarkt_data_cache)) {
      df_startdatums <- football_data %>%
        filter(Niveau == 1) %>%
        select(Competitie, Seizoen, Startdatum) %>%
        distinct()
      urls_tm <- read.csv("input/transfermarkt.csv") %>%
        inner_join(df_startdatums, by = c("Competitie", "Seizoen"))
      
      if(is.null(player_jsons_cache)) {
        player_jsons <- list()
      } else {
        player_jsons <- load(player_jsons_cache)
      }
      transfermarkt_data <- gather_transfermarkt_data(urls_tm, player_jsons_cache, current_season = FALSE)
      
      aggregated_transfermarkt_data <- aggregate_transfermarkt_data(transfermarkt_data)
      save(aggregated_transfermarkt_data, file = paste0("cache/aggregated_transfermarkt_data_",
                                                   Sys.Date(),
                                                   ".RData"))
    } else {
      load(aggregated_transfermarkt_data_cache)
    }
    
    all_models <- list()
    for(game_round in 0 : 30) {
      if(game_round > 0) {
        football_data_to_come <- football_data %>%
          filter(Aantal > game_round)
        aggregated_football_data_to_come <- aggregate_football_data(football_data_to_come)
        model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data_to_come)
        
        football_data_passed <- football_data %>%
          filter(Aantal <= game_round)
        aggregated_football_data_passed <- aggregate_football_data(football_data_passed)
        model_input <- add_in_season_info(model_input, aggregated_football_data_passed)
      } else model_input <- create_model_input(aggregated_football_data, aggregated_transfermarkt_data)
      
      all_models[[game_round + 1]] <- create_models_for_game_round(model_input)
    }
    save(all_models, file = paste0("cache/all_models_",
                                   Sys.Date(),
                                   ".RData"))
  } else {
    load(all_models_cache)
  }
  
  namen <- read.csv("input/namen.csv")
  
  if(is.null(aggregated_transfermarkt_data_new_cache)) {
    urls_tm_new <- read.csv("input/transfermarktnieuw.csv")
    urls_tm_new$Startdatum <- Sys.Date()
    transfermarkt_data_new <- gather_transfermarkt_data(urls_tm_new, current_season = TRUE)
    aggregated_transfermarkt_data_new <- aggregate_transfermarkt_data(transfermarkt_data_new)
    
    save(aggregated_transfermarkt_data_new, file = paste0("cache/aggregated_transfermarkt_data_new_",
                                                          Sys.Date(),
                                                          ".RData"))
  } else {
    load(aggregated_transfermarkt_data_new_cache)
  }
  
  urls_fd_new <- read.csv("input/footballdatanieuw.csv")
  football_data_new <- gather_football_data(urls_fd_new, 1)
  aggregated_football_data_new <- aggregate_football_data(football_data_new)
  
  aggregated_level_two_data <- aggregate_level_two_final_standings()
  
  prediction_with_shots <- use_model_on_new_data(aggregated_football_data, 
                                                 aggregated_transfermarkt_data_new, 
                                                 aggregated_football_data_new,
                                                 all_models,
                                                 with_shots = TRUE)
  prediction_without_shots <- use_model_on_new_data(aggregated_level_two_data, 
                                                    aggregated_transfermarkt_data_new, 
                                                    aggregated_football_data_new,
                                                    all_models,
                                                    with_shots = FALSE)
  prediction <- rbind(prediction_with_shots, prediction_without_shots)
  next_game_round_prediction <- predict_next_game_round(prediction)
  
  n_sims <- 10000
  for (competition in unique(prediction$Competitie)) {
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
    
    pb <- winProgressBar(title = paste(competition, "aan het simuleren"),
                         label = "Simuleren wordt gestart",
                         min = 0,
                         max = n_sims,
                         initial = 0)
    
    for (sim_nr in 1 : n_sims) {
      total_standings <- run_simulation(prediction_competition, matches_to_simulate, current_standings)    
      all_simulations[(n_teams * (sim_nr - 1) + 1) : (n_teams * sim_nr), 3 : 6] <- total_standings
      
      info <- sprintf("Simuleren %d%% voltooid", round((sim_nr / n_sims * 100)))
      setWinProgressBar(pb, sim_nr, label = info)
    }
    close(pb)
    
    results_table <- create_results_table(all_simulations)
    write.xlsx(results_table, paste0("output/kansentabel_", competition, "_on_", Sys.Date(), ".xlsx"))
  }
}