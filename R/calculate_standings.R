calculate_standings <- function(match_data) {
  standings_home <- match_data %>%
    group_by(HomeTeam) %>%
    summarise(Punten = sum(HPts),
              Doelpuntenvoor = sum(FTHG),
              Doelsaldo = Doelpuntenvoor - sum(FTAG)) %>%
    ungroup()
  standings_away <- match_data %>%
    group_by(AwayTeam) %>%
    summarise(Punten = sum(APts),
              Doelpuntenvoor = sum(FTAG),
              Doelsaldo = Doelpuntenvoor - sum(FTHG)) %>%
    ungroup()
  standings <- full_join(standings_home, standings_away, by=c("HomeTeam"="AwayTeam"))
  standings[is.na(standings)] <- 0
  standings <- standings %>%
    mutate(Team = HomeTeam,
           Punten = Punten.x + Punten.y,
           Doelpuntenvoor = Doelpuntenvoor.x + Doelpuntenvoor.y,
           Doelsaldo = Doelsaldo.x + Doelsaldo.y) %>%
    select(Punten, Doelpuntenvoor, Doelsaldo)
  
  return(standings)
}
  