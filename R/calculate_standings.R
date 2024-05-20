calculate_standings <- function(match_data) {
  standings_home <- match_data %>%
    group_by(HomeTeam) %>%
    summarise(Punten = sum(HPts),
              Doelpuntenvoor = sum(FTHG),
              Doelsaldo = Doelpuntenvoor - sum(FTAG)) %>%
    ungroup() %>%
    rename(Team = HomeTeam)
  standings_away <- match_data %>%
    group_by(AwayTeam) %>%
    summarise(Punten = sum(APts),
              Doelpuntenvoor = sum(FTAG),
              Doelsaldo = Doelpuntenvoor - sum(FTHG)) %>%
    ungroup() %>%
    rename(Team = AwayTeam)
  standings <- combine_standings(standings_home, standings_away)  
  return(standings)
}
  