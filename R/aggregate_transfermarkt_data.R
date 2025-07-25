aggregate_transfermarkt_data <- function(transfermarkt_data, market_value_adjustment = 2.8, n_players = 12, regularisation_param = 2.15) {
  flog.debug("Starts aggregating data from transfermarkt")
  aggregated_transfermarkt_data <- transfermarkt_data %>%
    mutate(Team = ifelse(Team == "CF Os Belenenses", "B SAD", as.character(Team)),
           Marktwaarde = Marktwaarde ^ (1 / market_value_adjustment)) %>%
    arrange(desc(Marktwaarde)) %>%
    group_by(Team, Competitie, Seizoen) %>%
    summarise(Marktwaarde = sum(Marktwaarde[1 : min(n(), n_players)])) %>%
    ungroup() %>%
    group_by(Competitie, Seizoen) %>%
    mutate(Marktwaarde = Marktwaarde / mean(Marktwaarde),
           Standaardafwijking = regularisation_param + 1 / sd(Marktwaarde),
           Marktwaarde = (Marktwaarde - 1) * Standaardafwijking + 1) %>%
    ungroup() %>%
    select(-Standaardafwijking)
  flog.debug("Finished aggregating data from transfermarkt")
  return(aggregated_transfermarkt_data)
}