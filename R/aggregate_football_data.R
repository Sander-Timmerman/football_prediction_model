aggregate_football_data <- function(football_data) {
  namen <- read.csv("input/namen.csv")
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
    rename("Team" = "HomeTeam") %>%
    mutate(Team = mgsub(as.character(Team), namen$Footballdata, namen$Transfermarkt))
    
  return(aggregated_football_data)
}