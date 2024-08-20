load_settings <- function(n_sims, write_results, competitions) {
  settings <- list()
  settings$n_sims <- n_sims
  settings$write_results <- write_results
  settings$current_season <- as.integer(format(Sys.Date() + 184, "%Y")) - 2000
  settings$competitions <- competitions
  return(settings)
}