
source("A0_header.R")

##########################
### Read the golf data ###
##########################

### read the 2015 & 2017 golf data
D15 = read_delim("data_rshot_2015.TXT", delim=";")
D17 = read_delim("data_rshot_2017.TXT", delim=";")
all(names(D15)==names(D17))
D = bind_rows(D15, D17)
dim(D)
D 

# ### read just the 2015 golf data
# D15 = read_delim("data_rshot_2015.TXT", delim=";")
# D = D15
# D = read_delim("data_rshot_2015.TXT", delim=";")

### clean the initial golf data
names(D) = str_remove_all(names(D), "^ ")
names(D) = str_remove_all(names(D), "^ ")
names(D) = str_replace_all(names(D), " ", "_")
names(D) = str_replace_all(names(D), "\\(", "_")
names(D) = str_replace_all(names(D), "\\)", "")
names(D) = str_replace_all(names(D), "\\/", "_")
names(D) = str_replace_all(names(D), "#", "num")
names(D)
D = D %>%
  mutate(
    Distance = as.numeric(Distance),
    Shot = as.numeric(Shot),
    Hole_Score = as.numeric(Hole_Score),
    shots_to_hole_post = Hole_Score - Shot,
    shots_to_hole_pre = shots_to_hole_post + 1,
  ) %>%
  relocate(Hole_Score, .after = Shot) %>%
  relocate(shots_to_hole_pre, .after = Hole_Score) %>%
  relocate(shots_to_hole_post, .after = shots_to_hole_pre) %>%
  rename(SG = Strokes_Gained_Baseline)
# View(D[1:1000,])

###########################################
### Player-Season Stroke-by-Stroke Data ###
###########################################

### use this code chunk if you want each player's data from all seasons lumped together under that player
{
  # df_strokes1 = 
  #   D %>%
  #   mutate(
  #     Player = paste(Player_First_Name, Player_Last_Name),
  #     stroke_info = paste0(Tour_Description,"_",Year,"_T",Tourn.num,"_C",Course_num,"_R",Round),
  #     hole = as.numeric(Hole),
  #   ) %>%
  #   select(
  #     Player_num, Player, From_Location_Scorer, stroke_info, hole,
  #     shots_to_hole_pre, shots_to_hole_post, SG, #Strokes_Gained_Category,
  #   ) %>%
  #   arrange(Player_num, stroke_info, hole)
  # df_strokes1
}

### use this code chunk if you want player-season data--each player's data separated for each season
{
  df_strokes1 = 
    D %>%
    mutate(
      Player = paste(Year, Player_First_Name, Player_Last_Name),
      stroke_info = paste0(Tour_Description,"_",Year,"_T",Tourn.num,"_C",Course_num,"_R",Round),
      hole = as.numeric(Hole),
    ) %>%
    group_by(Player_num,Year) %>%
    mutate(Player_num_1 = cur_group_id()) %>%
    ungroup() %>%
    select(-Player_num) %>%
    rename(Player_num = Player_num_1) %>%
    select(
      Player_num, Player, From_Location_Scorer, stroke_info, hole,
      shots_to_hole_pre, shots_to_hole_post, SG, #Strokes_Gained_Category,
    ) %>%
    arrange(Player_num, stroke_info, hole)
  df_strokes1
}

### tourn & num rounds in a tournament
df_strokes1A = 
  df_strokes1 %>%
  mutate(tourn = str_remove(stroke_info, "_R.*")) %>%
  relocate(tourn, .after=stroke_info) %>%
  group_by(tourn) %>%
  mutate(
    nRoundsInTourn = str_extract(stroke_info, "(?<=_R).*"),
    nRoundsInTourn = max(as.numeric(nRoundsInTourn)),
  ) %>%
  relocate(nRoundsInTourn, .after=tourn) %>%
  ungroup()
df_strokes1A

##########################################################
### Check absurdly high driving strokes gained numbers ###
##########################################################

# ### check absurd driving strokes gained numbers
# temp = df_strokes1A %>%
#   filter(From_Location_Scorer=="Tee Box") %>%
#   group_by(Player,Player_num,tourn) %>%
#   reframe(n=n(), SUE=sum(SG)) %>%
#   filter(n==72) %>%
#   arrange(-SUE)
# temp
# hist(temp$SUE)
# 
# temp %>%
#   filter(SUE > 12) %>%
#   distinct(tourn)
# 
# temp = 
#   df_strokes1A %>%
#   filter(Player_num == 957 & tourn == "PGA TOUR_2017_T350_C236" & From_Location_Scorer=="Tee Box")
# temp
# hist(temp$SG)
# nrow(temp)
# sum(temp$SG)
# 
# temp = 
#   df_strokes1A %>%
#   filter(Player_num == 925 & tourn == "PGA TOUR_2017_T350_C236" & From_Location_Scorer=="Tee Box")
# temp
# hist(temp$SG)
# nrow(temp)
# sum(temp$SG)

#############################################
### expected number of strokes to holeout ###
#############################################

# ### expected number of strokes to holeout, part 1
# df_strokes2 = 
#   df_strokes1A %>%
#   group_by(Player_num, stroke_info, hole,) %>%
#   mutate(
#     nShots = first(shots_to_hole_pre),
#     xstrokes_post = ifelse(shots_to_hole_post==0, 0, NA),
#     xstrokes_pre = xstrokes_post + SG +1,
#   ) %>%
#   ungroup() %>%
#   relocate(nShots, .after = hole) %>%
#   relocate(xstrokes_pre, .before = xstrokes_post)
# df_strokes2
# sum(is.na(df_strokes2$xstrokes_pre)) + sum(is.na(df_strokes2$xstrokes_post))
# 
# ### expected number of strokes to holeout, part 2
# ###     StrokesGained = xstrokesPre - xstrokesPost - 1
# ### --> xstrokesPre = StrokesGained + xstrokesPost + 1
# df_strokes3 = df_strokes2
# get_num_NA_relevant <- function(df_strokes3) {
#   sum(is.na(df_strokes3$xstrokes_pre)) + sum(is.na(df_strokes3$xstrokes_post))
# }
# num_NA_relevant = get_num_NA_relevant(df_strokes3)
# counter = 0
# while(num_NA_relevant > 0) {
#   print(counter)
#   df_strokes3 = 
#     df_strokes3 %>%
#     group_by(Player_num, stroke_info, hole,) %>%
#     mutate(
#       xstrokes_post = ifelse(is.na(xstrokes_post), lead(xstrokes_pre), xstrokes_post),
#       xstrokes_pre = xstrokes_post + SG +1,
#     ) %>%
#     ungroup() 
#   
#   if (get_num_NA_relevant(df_strokes3) == num_NA_relevant) {
#     break
#   } else {
#     num_NA_relevant = get_num_NA_relevant(df_strokes3)
#   }
#   counter = counter + 1
# } 
# df_strokes3
# get_num_NA_relevant(df_strokes3)

#####################
### Min num holes ###
#####################

#FIXME -- only if previous section (expected number of strokes to holeout) is commented out
df_strokes4 = 
  df_strokes1A %>%
  group_by(Player_num, stroke_info, hole) %>%
  mutate(nShots = first(shots_to_hole_pre)) %>%
  ungroup() %>%
  relocate(nShots, .after = hole) 

### each player's number of holes
df_nHoles = 
  df_strokes4 %>% 
  drop_na() %>%
  filter(From_Location_Scorer == "Tee Box") %>%
  group_by(Player_num) %>%
  reframe(nHoles = n())
df_nHoles

### keep players whose num holes exceeds a threshold
# min_nHoles = 100
min_nHoles = 150
df_strokes_f = 
  df_strokes4 %>%
  left_join(df_nHoles) %>%
  filter(nHoles >= min_nHoles) %>%
  drop_na() 
df_strokes_f

### visualize nHoles
plot_nHoles_hist = 
  df_strokes_f %>% 
  distinct(Player_num, nHoles) %>% 
  ggplot() + 
  geom_histogram(aes(x=nHoles),fill="black") + 
  geom_vline(xintercept=0,linetype="dashed",color="gray50",linewidth=1.5) +
  geom_vline(xintercept=100,linetype="dashed",color="gray50",linewidth=1.5) +
  # labs(title="Empirical Distribution of Golfers' Number of Holes") +
  xlab("Number of Holes") +
  ylab("Count")
# plot_nHoles_hist
ggsave(paste0("results_plot_nHoles_hist.png"),plot_nHoles_hist,width=5,height=4)

##################################################
### Bin into driving, putting, and approaching ###
##################################################

### scoring locations
table(df_strokes_f$From_Location_Scorer)

### bin by {driving, putting, approaching (all other shots)}
df_3grps_0 = 
  df_strokes_f %>%
  mutate(stroke_grp = case_when(
    From_Location_Scorer == "Green" ~ "Putting",
    From_Location_Scorer == "Tee Box" ~ "Driving",
    TRUE ~ "Approaching"
  )) %>%
  mutate(stroke_grp = factor(stroke_grp, levels = stroke_grp_levels))
df_3grps_0

### some holes for whatever reason have 0 strokes gained, remove those holes
df_3grps_SG0 = 
  df_3grps_0 %>%
  group_by(stroke_info) %>%
  reframe(prop_SG0 = sum(SG==0)/n()) %>%
  arrange(-prop_SG0) %>%
  mutate(remove_hole = prop_SG0 >= 0.10)
df_3grps_SG0

df_3grps_1 =
  df_3grps_0 %>%
  left_join(df_3grps_SG0) %>%
  filter(!remove_hole)
df_3grps_1

nrow(df_3grps_0)
nrow(df_3grps_1)

### aggregate by group, get SUE = Strokes Under Expected
###   for putting: putts under expected
###   for driving: strokes gained
###   for approaching: strokes under expected
df_3grps_2 = 
  df_3grps_1 %>%
  group_by(Player_num, Player, stroke_info, tourn, nRoundsInTourn, hole, nShots, nHoles, stroke_grp) %>%
  reframe(SUE = sum(SG), nStrokes=n()) %>%
  group_by(Player_num, stroke_grp) %>%
  mutate(nHoles_grp = n()) %>%
  ungroup() %>%
  arrange(Player_num, stroke_info, hole) %>%
  group_by(Player_num) %>%
  mutate(
    start_of_hole = stroke_info != lag(stroke_info) | hole != lag(hole),
    start_of_hole = ifelse(is.na(start_of_hole), TRUE, start_of_hole),
    player_hole_idx = cumsum(start_of_hole),
    nHoles = last(player_hole_idx),
  ) %>%
  ungroup() %>%
  relocate(player_hole_idx, .after=hole) %>%
  select(-start_of_hole) %>%
  relocate(nStrokes, .after = nShots) %>%
  rename(nStokesInHole = nShots) %>%
  relocate(nHoles_grp, .after=nHoles)
df_3grps_2

### check (if this is TRUE, nHoles is right)
(df_3grps_2 %>% filter(stroke_grp=="Driving") %>% reframe(p = sum(nHoles==nHoles_grp)/n()))$p == 1

### visualize nStrokes by group
plot_Nstrokes_hist =
  df_3grps_2 %>% ggplot(aes(x = nStrokes)) + facet_wrap(~stroke_grp) + 
  geom_bar(fill="black") + 
  scale_x_continuous(breaks=1:15) +
  scale_y_continuous(labels = label_comma()) +
  # labs(title="Empirical Distribution of the Number of Strokes on a Hole") +
  ylab("Count") + xlab("Number of Strokes")
# plot_Nstrokes_hist
ggsave(paste0("results_plot_Nstrokes_hist.png"),plot_Nstrokes_hist,width=8,height=2.5)

###################################################
### Remove bad tournaments with outlier numbers ###
###################################################

### check absurd driving strokes gained numbers
temp = 
  df_3grps_2 %>%
  filter(stroke_grp == "Driving") %>%
  group_by(Player,Player_num,tourn) %>%
  reframe(n=n(), SUE=sum(SUE)) %>%
  filter(n==72) %>%
  arrange(-SUE)
temp

df_outlier_tournaments = 
  temp %>%
  filter(SUE > 12) %>%
  distinct(tourn)
df_outlier_tournaments

df_3grps_3 = df_3grps_2 %>% filter(!(tourn %in% df_outlier_tournaments$tourn))
nrow(df_3grps_2)
nrow(df_3grps_3)

### final dataset
df_3grps_f = df_3grps_3

############################
### Dataset summary info ###
############################

### dataset
df_3grps_f

### summary statistics
s1=paste0("N = ", length(unique(df_3grps_f$Player_num))," golfers")
s2=paste0(nrow(df_3grps_f %>% distinct(Player_num, stroke_info, hole))," player-holes")
s3=paste0(nrow(df_3grps_f %>% filter(stroke_grp=="Driving")%>%distinct(Player_num, stroke_info, hole))," player-drives")
s4=paste0(nrow(df_3grps_f %>% filter(stroke_grp=="Putting")%>%distinct(Player_num, stroke_info, hole))," player-putts")
s5=paste0(nrow(df_3grps_f %>% filter(stroke_grp=="Approaching")%>%distinct(Player_num, stroke_info, hole))," player-approaches")
s6=paste0("unique seasons: ", paste0(unique(D$Year),collapse=", "))
s7=paste0("num unique seasons: ", length(unique(D$Year)))
s8=paste0("tours: ", paste0(unique(D$Tour_Description),collapse=", "))
# s9=paste0("num unique tournaments: ", length(unique(D$Tournament_Name)))
# s9=paste0("num unique tournaments: ", nrow(D %>% select(Tournament_Name, Year) %>% distinct()) )
s9=paste0("num unique tournaments: ", nrow(df_3grps_f %>% distinct(tourn)))
s10=paste0("min num holes: ", min_nHoles)
data_info_table = gt::gt(tibble(x = c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)))
# data_info_table
gt::gtsave(data_info_table, paste0("results_plot_summaryDataInfo.png"))

### save dataset
df_3grps_f
write_csv(df_3grps_f, paste0("data_holeByHole_byGrp.csv"))

############################
