library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)
library(xlsx)

rm(list = ls())
source.all("R")
load("cache/all_models.RData")
load("cache/aggregated_football_data.RData")

namen <- read.csv("input/namen.csv")

urls_tm_new <- read.csv("input/transfermarktnieuw.csv")
urls_tm_new$Startdatum <- Sys.Date()
transfermarkt_data_new <- gather_transfermarkt_data(urls_tm_new, current_season = TRUE)
aggregated_transfermarkt_data_new <- aggregate_transfermarkt_data(transfermarkt_data_new)

urls_fd_new <- read.csv("input/footballdatanieuw.csv")
football_data_new <- gather_football_data(urls_fd_new, 1)
aggregated_football_data_new <- aggregate_football_data(football_data_new)

aggregated_level_two_data <- aggregate_level_two_final_standings()

prediction_with_shots <- use_model_on_new_data(aggregated_football_data, 
                                               aggregated_transfermarkt_data_new, 
                                               aggregated_football_data_new, 
                                               with_shots = TRUE)
prediction_without_shots <- use_model_on_new_data(aggregated_level_two_data, 
                                                  aggregated_transfermarkt_data_new, 
                                                  aggregated_football_data_new, 
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
