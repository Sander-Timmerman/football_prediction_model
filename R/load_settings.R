load_settings <- function(n_sims, write_results, competitions, edit_blogger) {
  settings <- list()
  settings$n_sims <- n_sims
  settings$write_results <- write_results
  settings$current_season <- as.integer(format(Sys.Date() + 184, "%Y")) - 2000
  settings$competitions <- competitions
  if(edit_blogger) {
    if(!write_results) {
      edit_blogger <- FALSE
      flog.warn("The setting write_results is false, so blogger won't be edited")
    } else if(!file.exists("blogger_config.json")) {
      edit_blogger <- FALSE
      flog.warn("Blogger config not found, blogger won't be edited")
      }
  }
  settings$edit_blogger <- edit_blogger
  
  return(settings)
}