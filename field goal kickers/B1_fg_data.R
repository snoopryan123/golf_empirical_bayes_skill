
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

################
### get baseline field goal probabilities
################

### load field goal probability model from Brill Wyner fourth down paper
load_lm <- function(filename) {
  cm = readRDS(filename)
  attr(cm$terms, ".Environment") <- globalenv()
  if ( any(str_detect(class(cm), "glm")) ) {
    attr(cm$formula, ".Environment") <- globalenv()
  }
  return(cm)
}
fgp_model = load_lm("fg_model_b1.rds")
fgp_model

df_fg_1 = 
  df_fg_0 %>%
  mutate(kicker_player_id = factor(kicker_player_id)) %>%
  rename(yardline_100 = kick_distance) %>%
  mutate(kq = 0) %>%
  mutate(p_baseline = predict(fgp_model,.,type="response")) %>% 
  select(-kq)
df_fg_1

### plot
plot_fgp_baseline = 
  expand_grid(yardline_100 = 1:53, kq = 0) %>%
  mutate(p = predict(fgp_model, ., type = "response")) %>%
  ggplot(aes(x = yardline_100, y = p)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    name = "yards to opponent endzone",
    sec.axis = sec_axis(~ . + 17, name = "kick distance")
  ) +
  ylab("baseline field goal\nsuccess probability")
# plot_fgp_baseline
ggsave("results_fg_plot_fgProbBaseline.png", plot_fgp_baseline, width=6, height=4)

### save
write_csv(df_fg_1, "data_field_goals.csv")

