library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)
library(xlsx)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)

source.all("R")

output <- run_prediction(aggregated_football_data_cache = "cache/aggregated_football_data.RData",
                         all_models_cache = "cache/all_models.RData",
                         # aggregated_transfermarkt_data_new_cache = "cache/aggregated_transfermarkt_data_new_2024-03-04.RData",
                         n_sims = 10000,
                         write_results = TRUE)
View(output$results_table)
View(output$prediction)
View(output$next_game_round_prediction)