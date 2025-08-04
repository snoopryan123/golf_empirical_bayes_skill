
source("A0_header.R")
library(mgcv)

df_fg_0 = read_csv("data_field_goals.csv")
df_fg_0 = df_fg_0 %>% mutate(kicker_player_id = factor(kicker_player_id))
df_fg_0
table(df_fg_0$field_goal_result)

################
### fit baseline field goal probability model
################


# 2. If you have an “attempted” indicator, model selection (optional)
mod_att <- bam(
  attempted ~
    s(kick_distance, bs = "tp", k = 15) +
    s(kicker_player_id, bs = "re"),
  family   = binomial,
  data     = df_fg_0,
  method   = "fREML",
  discrete = TRUE
)

# 3. Fit the hierarchical FG‐success model
#    • global spline on kick_distance
#    • kicker‐specific deviations via fs spline
#    • random intercept per kicker_player_id
mod_fast <- bam(
  fg_success ~
    s(kick_distance, bs = "tp", k = 20) +
    s(kick_distance, kicker_player_id, bs = "fs", m = 1, k = 8) +
    s(kicker_player_id, bs = "re"),
  family   = binomial,
  data     = df_fg_0,
  method   = "fREML",
  discrete = TRUE
)

# 4. Quick timing
system.time({
  mod_fast <- bam(
    fg_success ~
      s(kick_distance, bs = "tp", k = 20) +
      s(kick_distance, kicker_player_id, bs = "fs", m = 1, k = 8) +
      s(kicker_player_id, bs = "re"),
    family   = binomial,
    data     = df_fg_0,
    method   = "fREML",
    discrete = TRUE
  )
})

# 5. Inspect
summary(mod_fast)



# library(mgcv)      # for GAMs
# 
# # Fit a Binomial GAM with:
# #    • a global smooth on distance
# #    • a kicker_player_id‐specific smooth on distance (bs="fs") that shrinks toward zero
# #    • a random intercept for kicker_player_id (bs="re")
# # mod <- gam(
# #   fg_success ~
# #     s(kick_distance, bs = "tp", k = 20) +                      # global thin‐plate spline
# #     s(kick_distance, by = factor(kicker_player_id), bs = "fs", m = 1, k = 10) + # one smooth per kicker_player_id, shared penalty
# #     s(kicker_player_id, bs = "re"),                                      # random intercept for kicker_player_id
# #   family = "binomial",
# #   data   = df_fg_0,
# #   method = "REML"
# # )
# # mod
# #
# # # Inspect
# # summary(mod$gam)
# # plot(mod$gam, pages = 1, rug = TRUE)
# 
# mod_fast <- bam(
#   fg_success ~
#     s(kick_distance, bs = "tp", k = 20) +
#     # factor-smooth for each kicker_player_id; shared penalty
#     s(kick_distance, kicker_player_id, bs = "fs", m = 1, k = 8) +
#     # random intercept for kicker_player_id
#     s(kicker_player_id, bs = "re"),
#   family   = binomial,
#   data     = df_fg_0,
#   method   = "fREML",
#   discrete = TRUE      # <-- huge speed boost for big data
# )
# mod_fast
# 
# ################
# ### plot
# ################
# 
# # 1. Create a grid of distances & kickers
# newd <- expand.grid(
#   kick_distance = seq(min(df_fg_0$kick_distance),
#                       max(df_fg_0$kick_distance),
#                       length = 200),
#   kicker_player_id        = unique(df_fg_0$kicker_player_id)
# )
# newd
# 
# # 2. Predict on the link scale (logit):
# #   • baseline only: exclude both the kicker_player_id smooth and the random intercept
# newd$eta_base <- predict(mod_fast,
#                          newdata = newd,
#                          type    = "link",
#                          exclude = c("s(kick_distance,kicker_player_id)","s(kicker_player_id)"))
# 
# #   • full (baseline + kicker_player_id shape + kicker_player_id intercept)
# newd$eta_full <- predict(mod_fast,
#                          newdata = newd,
#                          type    = "link")
# 
# # 3. Extract just the kicker_player_id intercept + shape component:
# newd$eta_kicker_dev <- newd$eta_full - newd$eta_base
# 
# # 4. Back-transform to probabilities
# newd <- newd %>%
#   mutate(
#     p_base      = plogis(eta_base),
#     p_full      = plogis(eta_full),
#     p_kicker_dev = plogis(eta_full) - plogis(eta_base)  # difference on prob scale
#   )
# 
# #
# newd = tibble(newd)
# newd = newd %>% left_join(df_fg_0 %>% distinct(kicker_player_id, kicker_player_name))
# newd
# 
# # 5. Visualize
# # 5a. Baseline curve (same for all kickers)
# ggplot(distinct(newd, kick_distance, p_base), aes(kick_distance, p_base)) +
#   geom_line(linewidth=1.2) +
#   labs(x = "Kick Distance", y = "P(field goal)",
#        title = "Global Baseline FG% vs Distance") +
#   theme_minimal()
# 
# # 5b. kicker_player_id-specific curves
# set.seed(42)  # for reproducibility
# kickers_15 <- sample(unique(df_fg_0$kicker_player_id), 15)
# newd %>%
#   filter(kicker_player_id %in% kickers_15) %>%
#   ggplot(aes(kick_distance, p_full, color = kicker_player_name)) +
#   geom_line(alpha = 0.6) +
#   labs(x = "Kick Distance", y = "P(field goal)",
#        title = "kicker_player_id-Specific FG% Curves") +
#   theme_minimal()
# 
# # 5c. Deviations from baseline
# newd %>%
#   filter(kicker_player_id %in% kickers_15) %>%
#   ggplot(aes(kick_distance, eta_kicker_dev, color = kicker_player_name)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_line(alpha = 0.6) +
#   labs(x = "Kick Distance", y = "Logit deviation",
#        title = "kicker_player_id-Specific Deviations (on logit scale)") +
#   theme_minimal()
# 
