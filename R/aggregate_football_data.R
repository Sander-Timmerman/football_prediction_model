aggregate_football_data <- function(football_data, namen) {
  flog.debug("Starts aggregating data from football_data")

  aggregated_football_data <- football_data %>%
    group_by(Competitie, Seizoen, Niveau) %>%
    mutate(Gemiddeldegoals = mean(FTHG),
           Gemiddeldeschoten = mean(HS),
           Gemiddeldeschotenopdoel = mean(HST)) %>%
    group_by(HomeTeam, Competitie, Seizoen, Niveau) %>%
    mutate(Aantalwedstrijden = max(Aantal) - min(Aantal) + 1) %>%
    summarise(Punten = sum(HPts) / max(Aantalwedstrijden),
              Doelpuntenvoor = sum(FTHG) / max(Aantalwedstrijden) / max(Gemiddeldegoals),
              Doelpuntentegen = sum(FTAG) / max(Aantalwedstrijden) / max(Gemiddeldegoals),
              Doelsaldo = Doelpuntenvoor - Doelpuntentegen,
              Goalratio = Doelpuntenvoor / sum(Doelpuntenvoor, Doelpuntentegen),
              Doelsom = Doelpuntenvoor + Doelpuntentegen,
              Schotenvoor = sum(HS) / max(Aantalwedstrijden) / max(Gemiddeldeschoten),
              Schotentegen = sum(AS) / max(Aantalwedstrijden) / max(Gemiddeldeschoten),
              Schotsaldo = Schotenvoor - Schotentegen,
              Schotratio = Schotenvoor / sum(Schotenvoor, Schotentegen),
              Schotsom = Schotenvoor + Schotentegen,
              Schotenopdoelvoor = sum(HST) / max(Aantalwedstrijden) / max(Gemiddeldeschotenopdoel),
              Schotenopdoeltegen = sum(AST) / max(Aantalwedstrijden) / max(Gemiddeldeschotenopdoel),
              Schotopdoelsaldo = Schotenopdoelvoor - Schotenopdoeltegen,
              Schotopdoelratio = Schotenopdoelvoor / sum(Schotenopdoelvoor, Schotenopdoeltegen),
              Schotenopdoelsom = Schotenopdoelvoor + Schotenopdoeltegen,
              Aantalwedstrijden = max(Aantalwedstrijden)) %>%
    ungroup() %>%
    select(-c(Doelpuntenvoor, Doelpuntentegen, Schotenvoor, Schotentegen, Schotenopdoelvoor, Schotenopdoeltegen)) %>%
    rename("Team" = "HomeTeam") %>%
    mutate(Team = mgsub(as.character(Team), namen$Football_data, namen$Transfermarkt))
  flog.debug("Finished aggregating data from football_data")
  return(aggregated_football_data)
}