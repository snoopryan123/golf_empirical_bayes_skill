
source("A0_header.R")

################
### get field goal data 
################

library(nflfastR)

seasons = 2010:2024
df_pbp_0 = load_pbp(seasons)
df_pbp_0

df_fg_0 = 
  df_pbp_0 %>%
  mutate(
    field_goal_attempt = ifelse(is.na(field_goal_attempt),0,field_goal_attempt),
    field_goal_attempt = as.logical(field_goal_attempt),
    field_goal_result = ifelse(is.na(field_goal_result),0,field_goal_result),
  ) %>%
  filter(field_goal_attempt) %>%
  select(
    kicker_player_id, kicker_player_name, season,
    field_goal_result, kick_distance, weather, wind
  ) %>%
  mutate(fg_success = as.numeric(field_goal_result=="made")) %>%
  relocate(fg_success, .before = field_goal_result)
df_fg_0

sum(is.na(df_fg_0$fg_success))
sum(is.na(df_fg_0$field_goal_result))

write_csv(df_fg_0, "data_field_goals.csv")





