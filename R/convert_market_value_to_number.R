convert_market_value_to_number <- function(market_values_string) {
  values <- as.numeric(mgsub(substr(market_values_string, 2, nchar(market_values_string)), c("m", "k"), c("", "")))
  multiplier <- as.numeric(mgsub(substr(market_values_string, nchar(market_values_string), nchar(market_values_string)), c("m", "k"), c("1000000", "1000")))
  market_values <- values * multiplier
  return(market_values)
}