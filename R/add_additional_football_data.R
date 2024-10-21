add_additional_football_data <- function(df_football_data, additional_football_data, competitie, seizoen, level) {
  additional_football_data <- filter(additional_football_data,
                                     Competition == competitie & Season == seizoen & Level == level)
  df_football_data <- df_football_data %>%
    full_join(additional_football_data, by = c("HomeTeam", "AwayTeam")) %>%
    mutate(Date = coalesce(Date.y, Date.x),
           FTHG = coalesce(FTHG.y, FTHG.x),
           FTAG = coalesce(FTAG.y, FTAG.x),
           FTR = coalesce(FTR.y, FTR.x),
           HS = coalesce(HS.y, HS.x),
           AS = coalesce(AS.y, AS.x),
           HST = coalesce(HST.y, HST.x),
           AST = coalesce(AST.y, AST.x)) %>%
    select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HS, AS, HST, AST) %>%
    arrange(as.Date(Date))
  return(df_football_data)
}