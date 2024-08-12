add_in_season_info <- function(model_input, aggregated_football_data_passed) {
  ncol_orig <- ncol(model_input)
  model_input_with_in_season <- left_join(model_input, 
                                          select(aggregated_football_data_passed, -Niveau),
                                          by = c("Team" = "Team",
                                                 "Competitie" = "Competitie",
                                                 "Seizoen" = "Seizoen"))
  model_input_with_in_season[is.na(model_input_with_in_season)] <- 0
  colnames(model_input_with_in_season)[(ncol_orig + 1) : (ncol(model_input_with_in_season) - 2)] <- paste0(colnames(model_input_with_in_season)[(ncol_orig + 1) : (ncol(model_input_with_in_season) - 2)],
                                                          "_in_seizoen")
  return(model_input_with_in_season)
}