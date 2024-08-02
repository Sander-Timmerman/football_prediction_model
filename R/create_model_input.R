create_model_input <- function(aggregated_football_data, aggregated_transfermarkt_data, aggregated_football_data_to_come, is_new_data) {
  aggregated_football_data_last_season <- aggregated_football_data %>%
    mutate(Seizoen = Seizoen + 1) %>%
    select(-Aantalwedstrijden)
  first_data_column <- 4
  
  if(!is_new_data) {
    aggregated_football_data_this_season <- aggregated_football_data_to_come %>%
      filter(Niveau == 1) %>%
      select(Team, Competitie, Seizoen, Punten, Doelsom)
    
    model_input <- inner_join(aggregated_football_data_this_season,
                              aggregated_football_data_last_season,
                              by = c("Team" = "Team",
                                     "Competitie" = "Competitie",
                                     "Seizoen" = "Seizoen"))
    colnames(model_input)[first_data_column : (first_data_column + 1)] <- paste0(colnames(model_input)[first_data_column : (first_data_column + 1)], "_dit_seizoen")
    colnames(model_input) <- mgsub(colnames(model_input),
                                   c(".x", ".y"),
                                   c("", ""))
    first_data_column <- first_data_column + 2
  } else model_input <- aggregated_football_data_last_season
  
  colnames(model_input)[first_data_column : ncol(model_input)] <- paste0(colnames(model_input)[first_data_column : ncol(model_input)], "_vorig_seizoen")
  
  model_input <- model_input %>%
    inner_join(aggregated_transfermarkt_data,
               by = c("Team" = "Team",
                      "Competitie" = "Competitie",
                      "Seizoen" = "Seizoen"))
  return(model_input)
}