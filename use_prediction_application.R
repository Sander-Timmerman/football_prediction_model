library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)
library(xlsx)

rm(list = ls())
options(dplyr.summarise.inform = FALSE)
flog.threshold(INFO)

source.all("R")

output <- run_prediction_application(n_sims = 10000,
                                     write_results = TRUE)
View(output$results_table)
View(output$prediction)
View(output$next_game_round_prediction)