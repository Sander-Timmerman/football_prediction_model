create_model_input <- function(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data_to_come = NULL) {
  if(is.null(aggregated_football_data_to_come)) {
    aggregated_football_data_to_come <- aggregated_football_data
  }
  
  aggregated_football_data_this_season <- aggregated_football_data_to_come %>%
    filter(Niveau == 1) %>%
    select(Team, Competitie, Seizoen, Punten, Doelsom)
  aggregated_football_data_last_season <- aggregated_football_data %>%
    mutate(Seizoen = Seizoen + 1) %>%
    select(-Aantalwedstrijden)
  
  model_input <- inner_join(aggregated_football_data_this_season,
                            aggregated_football_data_last_season,
                            by = c("Team" = "Team",
                                   "Competitie" = "Competitie",
                                   "Seizoen" = "Seizoen")) %>%
    inner_join(aggregated_transfermarkt_data)

  colnames(model_input)[4 : 5] <- paste0(colnames(model_input)[4 : 5], "_dit_seizoen")
  colnames(model_input)[6 : (ncol(model_input) - 1)] <- paste0(colnames(model_input)[6 : (ncol(model_input) - 1)], "_vorig_seizoen")
  colnames(model_input) <- mgsub(colnames(model_input),
                                 c(".x", ".y"),
                                 c("", ""))
  return(model_input)
}