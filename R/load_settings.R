load_settings <- function(n_sims, write_results) {
  settings <- list()
  settings$n_sims <- n_sims
  settings$write_results <- write_results
  return(settings)
}