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

output <- run_prediction_application(football_data_cache = 8,
                                     aggregated_football_data_cache = 8,
                                     n_sims = 1,
                                     write_results = TRUE)
# View(output$results_table)
# View(output$prediction)
# View(output$next_game_round_prediction)