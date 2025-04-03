library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)
library(xlsx)
library(gt)
library(uuid)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)
flog.threshold(INFO)

source.all("R")

output <- run_prediction_application(football_data_cache = 649,
                                     aggregated_football_data_cache = 651,
                                     player_jsons_cache = 51,
                                     transfermarkt_data_cache = 51,
                                     transfermarkt_data_new_cache = 771,
                                     all_final_standings_cache = 273,
                                     all_models_cache = 704, #297,
                                     n_sims = 1,
                                     write_results = TRUE,
                                     edit_blogger = TRUE)