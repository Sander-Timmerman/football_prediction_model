library(dplyr)
library(miceadds)
library(mgsub)
library(futile.logger)
library(rvest)
library(rjson)

rm(list = ls())
source.all("R")

urls_fd_first_level <- read.csv("input/footballdata2.csv")
urls_fd_second_level <- read.csv("input/footballdataniveautwee.csv")

football_data_first_level <- gather_football_data(urls_fd_first_level, 1)
football_data_second_level <- gather_football_data(urls_fd_second_level, 2)
football_data <- rbind(football_data_first_level, football_data_second_level)

aggregated_football_data <- aggregate_football_data(football_data)

df_startdatums <- football_data %>%
  filter(Niveau == 1) %>%
  select(Competitie, Seizoen, Startdatum) %>%
  distinct()
urls_tm <- read.csv("input/transfermarkt.csv") %>%
  inner_join(df_startdatums, by = c("Competitie", "Seizoen"))
transfermarkt_data <- gather_transfermarkt_data(urls_tm, current_season = FALSE)

aggregated_transfermarkt_data <- aggregate_transfermarkt_data(transfermarkt_data)

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