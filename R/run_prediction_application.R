run_prediction_application <- function(football_data_cache = NULL,
                                       aggregated_football_data_cache = NULL, 
                                       all_models_cache = NULL, 
                                       player_jsons_cache = NULL,
                                       transfermarkt_data_cache = NULL,
                                       transfermarkt_data_new_cache = NULL,
                                       all_final_standings_cache = NULL,
                                       n_sims = 10000,
                                       write_results = TRUE,
                                       edit_blogger = FALSE,
                                       competitions = "all") {
  output <- tryCatch(
    {
      param_json <- convert_parameters_to_json(match.call())
      run_number <- prepare_run(param_json, write_results)
      flog.info(paste0("Start run with run number ", run_number))
      all_cache_numbers <- load_cache_numbers(football_data_cache,
                                              aggregated_football_data_cache, 
                                              all_models_cache, 
                                              player_jsons_cache,
                                              transfermarkt_data_cache,
                                              transfermarkt_data_new_cache,
                                              all_final_standings_cache)
      local_input <- load_local_input()
      settings <- load_settings(n_sims, write_results, competitions, edit_blogger)
      if(settings$edit_blogger) {
        blogger_info <- configure_google_token()
      } else blogger_info <- NULL
      
      output <- run_prediction(all_cache_numbers, local_input, settings, blogger_info, run_number)
      flog.info("Application run finished successfully")
      update_run_history(run_number, success = TRUE)
      
      output
    },
    error = function(e) {
      flog.fatal(paste0("Application crashed because of the following error: ", e))
      update_run_history(run_number, success = FALSE)
      return(NULL)
    }
  )
  return(output)
}