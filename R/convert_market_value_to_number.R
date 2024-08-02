convert_market_value_to_number <- function(market_values_string) {
  values <- as.numeric(mgsub(substr(market_values_string, 2, nchar(market_values_string)), 
                             c("m", "k", "\\s"), 
                             c("", "", "")))
  multiplier <- as.numeric(mgsub(gsub("[^a-zA-Z]", "", market_values_string),
                                 c("m", "k"),
                                 c(1000000, 1000)))
  market_values <- values * multiplier
  market_values[is.na(market_values)] <- 0
  return(market_values)
}