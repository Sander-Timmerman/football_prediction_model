library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)
library(xlsx)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)
flog.threshold(TRACE)

source.all("R")

output <- run_prediction(aggregated_football_data_cache = "cache/aggregated_football_data.RData",
                         all_models_cache = "cache/all_models.RData",
                         player_jsons_cache = "cache/player_jsons_2023-11-18.RData",
                         transfermarkt_data_cache = "cache/transfermarkt_data_2023-11-20-1.RData",
                         aggregated_transfermarkt_data_new_cache = "cache/aggregated_transfermarkt_data_new_2024-05-18.RData",
                         n_sims = 1,
                         write_results = FALSE)
View(output$results_table)
View(output$prediction)
View(output$next_game_round_prediction)