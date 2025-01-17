create_model_with_and_without_shots <- function(model_input, number_games, perfect_variance_one_game, old_model = NULL, fixed_model = NULL, threshold = 0.01, non_removable_variables = NULL) {
  models <- list()
  for(with_or_without in c("with_shots", "without_shots")) {
    if(!is.null(old_model)) {
      model_input_with_without <- model_input[-(which(!(colnames(model_input) %in% 
                                                   names(old_model[[with_or_without]]$coefficients)))[-1])]
    } else {
      model_input_with_without <- model_input
    }
    
    non_removable_variables <- c(non_removable_variables, 
                                 names(fixed_model[[with_or_without]]$coefficients)[-1])
    
    if(with_or_without == "without_shots") {
      shots_last_season <- which(substr(colnames(model_input_with_without), 1, 5) == "Schot" & 
                                   substr(colnames(model_input_with_without), 
                                          nchar(colnames(model_input_with_without)) - 13, 
                                          nchar(colnames(model_input_with_without))) == "_vorig_seizoen")
      if(length(shots_last_season) > 0) {
        model_input_with_without <- model_input_with_without[-shots_last_season]
      } else {
        model_input_with_without <- model_input_with_without
      }
    }
    
    number_games_this_model <- number_games[which(complete.cases(model_input_with_without))]
    model_input_with_without <- na.omit(model_input_with_without)
    
    model_with_without <- fit_model(model_input_with_without, threshold, non_removable_variables)
    performance <- calculate_model_performance(model_with_without, number_games_this_model)
    model_with_without$performance <- performance[1]
    model_with_without$performance_sd <- performance[2]
    models[[with_or_without]] <- model_with_without
  }
  return(models)
}