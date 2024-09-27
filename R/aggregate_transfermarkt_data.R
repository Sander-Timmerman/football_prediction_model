aggregate_transfermarkt_data <- function(transfermarkt_data) {
  flog.info("Starts aggregating data from transfermarkt")
  aggregated_transfermarkt_data <- transfermarkt_data %>%
    mutate(Team = ifelse(Team == "CF Os Belenenses", "B SAD", as.character(Team)),
           Team = ifelse(Team == "GD Estoril Praia", "Estoril Praia", as.character(Team)),
           Marktwaarde = Marktwaarde ^ (1 / 3)) %>%
    arrange(desc(Marktwaarde)) %>%
    group_by(Team, Competitie, Seizoen) %>%
    summarise(Marktwaarde = sum(Marktwaarde[1 : min(n(), 20)])) %>%
    ungroup() %>%
    group_by(Competitie, Seizoen) %>%
    mutate(Marktwaarde = Marktwaarde / mean(Marktwaarde),
           Standaardafwijking = (sd(Marktwaarde) * 1.23727 * 0.4277 + 0.2138) / (sd(Marktwaarde) * 1.23727),
           Marktwaarde = (Marktwaarde - 1) * Standaardafwijking + 1) %>%
    ungroup() %>%
    select(-Standaardafwijking)
  return(aggregated_transfermarkt_data)
}