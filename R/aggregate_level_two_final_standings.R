aggregate_level_two_final_standings <- function() {
  aggregated_level_two_data <- read.csv("input/eindstand.csv") %>%
    group_by(Competitie, Seizoen) %>%
    mutate(Niveau = 2,
           Punten = Punten / Aantalwedstrijden,
           Doelpuntenvoor = Doelpuntenvoor / Aantalwedstrijden / mean(Doelpuntenvoor) * mean(Aantalwedstrijden),
           Doelpuntentegen = Doelpuntentegen / Aantalwedstrijden / mean(Doelpuntentegen) * mean(Aantalwedstrijden),
           Doelsaldo = Doelpuntenvoor - Doelpuntentegen,
           Goalratio = Doelpuntenvoor / (Doelpuntenvoor + Doelpuntentegen),
           Doelsom = Doelpuntenvoor + Doelpuntentegen) %>%
    ungroup()
  return(aggregated_level_two_data)
}