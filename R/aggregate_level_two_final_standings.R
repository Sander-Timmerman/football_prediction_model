aggregate_level_two_final_standings <- function(data_source_info, is_current_season) {
  aggregated_level_two_data <- gather_level_two_final_standings(data_source_info, is_current_season) %>%
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