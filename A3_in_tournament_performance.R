
source("A0_header.R")

#################
### read data ###
#################

df0 = read_csv("data_holeByHole_byGrp.csv", show_col_types = F)
df0$stroke_grp = factor(df0$stroke_grp, levels=stroke_grp_levels)
df0

#########################################################
### Decompose final tournament strokes  (standardized) 
### as the sum of the stroked gained (standardized) in 
### driving, approach and putting
#########################################################

### record which players played in all rounds of the tournament
### (i.e., made the cut)
df_inTourn_0 = 
  df0 %>%
  group_by(Player_num, Player, tourn) %>%
  mutate(
    nRounds_playerTourn = str_extract(stroke_info, "(?<=_R).*"),
    nRounds_playerTourn = max(as.numeric(nRounds_playerTourn)),
  ) %>%
  relocate(nRounds_playerTourn, .after=nRoundsInTourn)  %>%
  ungroup() %>%
  mutate(playedInAllRds = nRoundsInTourn == nRounds_playerTourn)
df_inTourn_0

### get numStrokes and sumStrokesGained for each player-tournament
df_inTourn_1 = 
  df_inTourn_0 %>%
  group_by(Player_num, Player, tourn, stroke_grp, playedInAllRds) %>%
  reframe(
    nStrokes = sum(nStrokes),
    SUE = sum(SUE),
  ) %>%
  group_by(Player_num, Player, tourn) %>%
  mutate(nStrokesTourn = sum(nStrokes)) %>%
  ungroup() 
df_inTourn_1

### pivot
df_inTourn_2 = 
  df_inTourn_1 %>%
  select(-nStrokes) %>%
  pivot_wider(names_from = stroke_grp, values_from = SUE, names_prefix = "SUE_") %>%
  arrange(tourn, nStrokesTourn)
df_inTourn_2

### proportion of variation in number of strokes in the tournament
### explained by sum of strokes gained in Driving, Approaching, and Putting
lm_inTourn <- lm(
  scale(nStrokesTourn) ~ scale(SUE_Driving) + scale(SUE_Approaching) + scale(SUE_Putting), 
  data = df_inTourn_2
)
round(lm_inTourn$coefficients,3)

### get TOP and MIDDLE player buckets 
M = 10

df_inTourn_isTop = 
  df_inTourn_2 %>% filter(playedInAllRds) %>% # made the cut
  group_by(tourn) %>%
  mutate(
    rank = rank(nStrokesTourn, ties.method = "min"),
    top = rank <= M,
  ) %>%
  ungroup() %>%
  filter(top) %>%
  distinct(Player_num, Player, tourn, top)
df_inTourn_isTop

df_inTourn_isMiddle = 
  df_inTourn_2 %>% # all golfers 
  group_by(tourn) %>%
  mutate(
    rank = rank(nStrokesTourn, ties.method = "min"),
    n_players = n(),
    middle_L = floor(n_players / 2) - (M-1),
    middle_U = floor(n_players / 2) + M,
    middle = rank >= middle_L & rank <= middle_U
  ) %>%
  ungroup() %>%
  # select(-n_players, -middle_L, -middle_U) %>%
  filter(middle) %>%
  distinct(Player_num, Player, tourn, middle)
df_inTourn_isMiddle

df_inTourn_topMid = 
  df_inTourn_2 %>%
  left_join(df_inTourn_isTop) %>%
  left_join(df_inTourn_isMiddle) %>%
  mutate(
    performance_group = case_when(
      top ~ paste0("Top ", M),
      middle ~ paste0("Middle ", M),
      TRUE ~ "Other",
    )
  ) %>%
  select(-top, -middle)
df_inTourn_topMid

# Step 5: Boxplots comparing Top vs Middle Golfers for each component
plot_boxplot_topMid = 
  df_inTourn_topMid %>%
  pivot_longer(
    c(SUE_Driving, SUE_Approaching, SUE_Putting), 
    names_to="stroke_category",
    values_to = "SUE"
  ) %>%
  mutate(
    stroke_category=str_remove(stroke_category, "SUE_"),
    stroke_category=factor(stroke_category,levels = stroke_grp_levels)
  ) %>%
  filter(performance_group != "Other") %>%
  ggplot(aes(x = performance_group, y = SUE))+
  facet_wrap(~stroke_category) +
  geom_hline(yintercept=0, color="gray60",linetype="solid",linewidth=1) +
  geom_boxplot() +
  # labs(caption="Distribution of strokes under expected (sum of strokes gained) in a tournament") +
  xlab("Performance Group") +
  ylab("Strokes Under Expected")
# plot_boxplot_topMid
ggsave("results_plot_boxplot_topMid.png",width=10,height=4)




