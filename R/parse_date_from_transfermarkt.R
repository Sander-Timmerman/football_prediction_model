parse_date_from_transfermarkt <- function(date_string, maanden, unknown_values_as_nas = FALSE) {
  date_string[date_string %in% c("-", "")] <- "01.01.1900"
  date <- as.Date(date_string, format = "%d.%m.%Y")
  return(date)
} 