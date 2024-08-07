aggregate_level_two_final_standings <- function(all_final_standings_cache, data_source_info, is_current_season, run_number) {
  flog.info("Starts gathering and aggregating the final standings of the second level of last season")
  aggregated_level_two_data <- use_function_with_caching(all_final_standings_cache, 
                                                         "all_final_standings",
                                                         run_number,
                                                         gather_level_two_final_standings, 
                                                         data_source_info, 
                                                         is_current_season) %>%
    group_by(Competitie, Seizoen) %>%
    mutate(Niveau = 2,
           Punten = Punten / Aantalwedstrijden,
           Doelpuntenvoor = Doelpuntenvoor / Aantalwedstrijden / mean(Doelpuntenvoor) * mean(Aantalwedstrijden),
           Doelpuntentegen = Doelpuntentegen / Aantalwedstrijden / mean(Doelpuntentegen) * mean(Aantalwedstrijden),
           Doelsaldo = Doelpuntenvoor - Doelpuntentegen,
           Goalratio = Doelpuntenvoor / (Doelpuntenvoor + Doelpuntentegen),
           Doelsom = Doelpuntenvoor + Doelpuntentegen) %>%
    ungroup()
  flog.info("Finished gathering and aggregating the final standings of the second level of last season")
  return(aggregated_level_two_data)
}