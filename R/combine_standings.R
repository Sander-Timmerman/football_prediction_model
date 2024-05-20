combine_standings <- function(standings_1, standings_2) {
  standings <- full_join(standings_1, standings_2, by="Team")
  standings[is.na(standings)] <- 0
  standings <- standings %>%
    mutate(Punten = Punten.x + Punten.y,
           Doelpuntenvoor = Doelpuntenvoor.x + Doelpuntenvoor.y,
           Doelsaldo = Doelsaldo.x + Doelsaldo.y) %>%
    arrange(Team) %>%
    select(Team, Punten, Doelpuntenvoor, Doelsaldo)
  return(standings)
}