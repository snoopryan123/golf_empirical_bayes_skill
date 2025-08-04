
source("../A0_header.R")

### load data
df_fg_1 = read_csv("data_field_goals.csv")
df_fg_1

df_fg_2 = 
  df_fg_1 %>%
  select(kicker_player_id, kicker_player_name, season, fg_success, p_baseline) %>%
  rename(kicker_id = kicker_player_id, kicker_name = kicker_player_name) %>%
  group_by(kicker_id, season) %>%
  mutate(Player_num = cur_group_id(), N = n()) %>%
  ungroup() %>%
  arrange(Player_num) %>%
  mutate(Player = paste0(season," ",kicker_name)) %>%
  mutate(X = fg_success - p_baseline)
df_fg_2

hist(df_fg_2$X)

M = 20 # min num field goal attempts
table(df_fg_2$N)
df0 = df_fg_2 %>% filter(N >= M)
print(c(nrow(df_fg_2), nrow(df0)))

print(paste0("there are ", length(unique(df0$Player_num)), " unique kickers"))

######################################
### Empirical Bayes Skill Function ###
######################################

fit_params.EB.mle <- function(epsilon=1e-4, df0) {
  ### first, fit the MLE hyperparams mu.hat, tau.sq.hat, sig.sq.i.hat
  ### initialize
  iter = 1
  df = df0 %>% select(Player_num, X) %>% drop_na()
  mu.hat = df %>% group_by(Player_num) %>% reframe(mu = mean(X,na.rm=T)) %>% reframe(mu = mean(mu,na.rm=T))
  full.var = var(df$X)
  params.df = df %>% 
    group_by(Player_num) %>% 
    reframe(sig.sq.i.hat = var(X)) %>% 
    drop_na(sig.sq.i.hat) %>%
    mutate(sig.sq.i.hat = median(sig.sq.i.hat))
  params.df$mu.hat = mu.hat$mu
  params.df$tau.sq.hat = full.var - mean(params.df$sig.sq.i.hat)
  params.df
  train.df = df %>% left_join(params.df) %>% drop_na()
  train.df
  
  ### iteratively solve for MLE mu.hat, tau.sq.hat, sig.sq.i.hat
  while (TRUE) {
    print(paste0("running iteration ", iter))
    ### save previous values
    train.df.prev = train.df
    ### solve for mu.hat
    mu.hat = train.df %>%
      reframe(
        t1 = sum(X / (sig.sq.i.hat + tau.sq.hat), na.rm=T),
        t2 = sum(1 / (sig.sq.i.hat + tau.sq.hat), na.rm=T),
      ) %>%
      mutate(
        mu.hat = t1/t2
      ) 
    mu.hat = mu.hat$mu.hat
    mu.hat
    train.df$mu.hat = mu.hat
    ### solve for tau.sq.hat
    solve.for.tau.sq <- function(tau.sq) {
      temp.df = 
        train.df %>% 
        select(-tau.sq.hat) %>% 
        mutate(
          t1 = 1 / (sig.sq.i.hat + tau.sq),
          t1.sq = t1^2,
          dd = (X - mu.hat)^2,
          t2 = dd*t1.sq,
          diff = t1 - t2
        ) 
      temp.df
      (temp.df %>% reframe(ans = sum(diff)))$ans
    }
    # tau.sq.hat = uniroot(solve.for.tau.sq, c(0.0001,50000000))
    tau.sq.hat = uniroot(solve.for.tau.sq, c(0.00000001,50000000))
    tau.sq.hat = tau.sq.hat$root
    tau.sq.hat
    train.df$tau.sq.hat = tau.sq.hat
    ### solve for sig.sq.p
    golfers = unique(train.df$Player_num)
    sig.sq.p.vec = numeric(length(golfers))
    for (i in 1:length(golfers)) {
      if (i %% 50 == 0) print(paste0("computing sig.sq.i.hat for golfer i=",i,"/",length(golfers)))
      pit_i = golfers[i]
      train.df_i = train.df %>% filter(Player_num == pit_i)
      
      solve.for.sig.sq <- function(sig.sq.p) {
        temp.df = 
          train.df_i %>% 
          select(-sig.sq.i.hat) %>% 
          mutate(
            dd = (X - mu.hat)^2,
            t1 = 1 / (sig.sq.p + tau.sq.hat),
            t1.sq = t1^2,
            t2 = dd*t1.sq,
            diff = t1 - t2
          ) 
        temp.df
        (temp.df %>% reframe(ans = sum(diff)))$ans
      }
      # solve.for.sig.sq(0.01)
      # sig.sq.i.hat = uniroot(solve.for.sig.sq, c(0.0001,1))
      sig.sq.i.hat = uniroot(solve.for.sig.sq, c(0.000000001,50))
      sig.sq.i.hat = sig.sq.i.hat$root
      sig.sq.p.vec[i] = sig.sq.i.hat
    }
    train.df = train.df %>% select(-sig.sq.i.hat) %>% left_join(
      tibble(Player_num = golfers, sig.sq.i.hat = sig.sq.p.vec)
    )
    ### check convergence
    mu.hat.conv = abs(unique(train.df$mu.hat) - unique(train.df.prev$mu.hat))
    tau.sq.hat.conv = abs(unique(train.df$tau.sq.hat) - unique(train.df.prev$tau.sq.hat))
    sig.sq.hat.df.prev = train.df.prev %>% group_by(Player_num) %>% reframe(sig.sq.i.hat = unique(sig.sq.i.hat))
    sig.sq.hat.df = train.df %>% group_by(Player_num) %>% reframe(sig.sq.i.hat = unique(sig.sq.i.hat))
    sig.sq.hat.conv = max(abs(sig.sq.hat.df$sig.sq.i.hat - sig.sq.hat.df.prev$sig.sq.i.hat))
    conv.num = max(c(mu.hat.conv, tau.sq.hat.conv, sig.sq.hat.conv))
    print(paste0("conv.num = ", conv.num))
    if (conv.num < epsilon) {
      break
    } 
    ### increment 
    iter = iter + 1
  }
  ### dataframe of fitted MLE hyperparams 
  df.hyperparams = train.df %>% distinct(Player_num, mu.hat, tau.sq.hat, sig.sq.i.hat)
  df.hyperparams
  
  ### now, get the empirical Bayes golfer quality estimates
  df.mu.hat.i = train.df %>%
    group_by(Player_num) %>%
    reframe(
      tA = sum(X)/sig.sq.i.hat,
      tB = mu.hat/tau.sq.hat,
      tC = n()/sig.sq.i.hat,
      tD = 1/tau.sq.hat
    ) %>%
    distinct() %>%
    group_by(Player_num) %>%
    reframe(
      V.hat.i = 1/(tC + tD),
      mu.hat.i = (tA + tB)*V.hat.i,
    )
  df.mu.hat.i
  
  ### result dataframe
  df.result = df.mu.hat.i %>% left_join(df.hyperparams)
  df.result
  df.result %>% arrange(-mu.hat.i)
}

###############
### Results ###
###############

alphas = c(0.01, 0.05, 0.10, 0.15) #, 0.20, 0.25, 0.5)
df.EB.results.full = tibble()
df_pvals_BH.full = tibble()
df_nsig.full = tibble()

### fit the empirical bayes parameters
df_EB_fit = fit_params.EB.mle(df0=df0)
df_EB_fit
mu.hat.overall = unique(df_EB_fit$mu.hat)
mu.hat.overall

### cleaned Empirical Bayes results dataframe
df.EB.results.full = 
  df_EB_fit %>%
  select(Player_num, mu.hat.i, V.hat.i) %>%
  left_join(
    df0 %>% distinct(Player_num, Player, N)
  ) %>%
  left_join(
    df0 %>% group_by(Player,Player_num) %>% reframe(mu.hat.MLE.i = mean(X))
  ) %>%
  relocate(Player, .after=Player_num) %>%
  mutate(
    p_val = 2*pnorm(-abs(mu.hat.i - mu.hat.overall)/sqrt(V.hat.i))
  ) %>%
  arrange(-mu.hat.i, p_val) 
df.EB.results.full

### Multiple Testing: Benjamini-Hochberg (BH) to control the False Discovery Rate (FDR)
nsig_golfers = c()
df_pvals_BH = tibble()
for (alpha in alphas) {
  ### Multiple Testing: Benjamini-Hochberg (BH) to control the False Discovery Rate (FDR)
  p_vals = df.EB.results.full$p_val
  p_vals_adjusted_BH = p.adjust(p_vals, method = "BH", n=length(p_vals))
  significant_putters = which(p_vals_adjusted_BH <= alpha) # Identify significant discoveries at FDR 0.05
  nsig = length(significant_putters)
  nsig_golfers = c(nsig_golfers, nsig)
  
  ###
  N <- length(p_vals)
  df_pvals_BH_alpha = 
    tibble(
      alpha = alpha,
      rank = 1:N,
      pval = sort(p_vals),
      bh_threshold = alpha * (1:N) / N,
    ) 
  df_pvals_BH = bind_rows(df_pvals_BH, df_pvals_BH_alpha)
  df_pvals_BH.full = bind_rows(df_pvals_BH.full, df_pvals_BH)
}
df_nsig.full = tibble(alpha = alphas, nsig = nsig_golfers)
df_nsig.full

################
### Plotting ###
################

###
df.EB.results.full
df_pvals_BH.full
df_nsig.full

### contextualize the effect sizes
gt_effect_sizes = 
  df.EB.results.full %>%
  reframe(
    qL = quantile(mu.hat.i, 0.05),
    qU = quantile(mu.hat.i, 0.95)
  ) %>%
  mutate(
    delta_kick = qU - qL,
  ) %>%
  gt() %>%
  fmt_number("qL", decimals=3) %>%
  fmt_number("qU", decimals=3) %>%
  fmt_number("delta_kick", decimals=3) 
# gt_effect_sizes
gtsave(gt_effect_sizes, "results_fg_plot_effectSizes.png")

### visualize
num_kickers = nrow(df.EB.results.full)
plot_EB_estimates = 
  df.EB.results.full %>%
  ggplot(aes(x = mu.hat.i)) +
  geom_histogram(fill="black") +
  geom_vline(xintercept=mu.hat.overall,linetype="dashed",color="gray50",linewidth=1.5) +
  xlab(TeX("Empirical Bayes estimate $\\hat{\\mu}^{(EB)}_{i}$")) +
  labs(title = paste0("Distribution of Estimated Kicker Skills")) +
  ylab("Count")
# plot_EB_estimates
ggsave(paste0("results_fg_plot_EB.png"), plot_EB_estimates, width=6, height=4)

### visualize: how much shrinkage?
plot_EB_shrinkage = 
  df.EB.results.full  %>%
  ggplot(aes(x=mu.hat.MLE.i, y=mu.hat.i, color=N, size=N)) +
  xlab(TeX("Observed Mean $\\hat{\\mu}^{(MLE)}_{i}$")) +
  ylab(TeX("Empirical Bayes estimate $\\hat{\\mu}^{(EB)}_{i}$")) +
  geom_hline(yintercept=mu.hat.overall,linetype="dashed",color="gray60",linewidth=1) +
  geom_abline(intercept=0, slope=1, linewidth=1, linetype="dashed", color="gray60") +
  geom_abline(intercept=0, slope=0, linewidth=0.5, linetype="solid", color="gray70") +
  geom_point(alpha=0.6) +
  labs(title = paste0("Visualizing Shrinkage in Estimating Kicker Skills"))
# plot_EB_shrinkage
ggsave(paste0("results_fg_plot_EB_shrinkage.png"), plot_EB_shrinkage, width=7, height=5)

### plot Benjamini Hochberg
plot_BH = 
  df_pvals_BH.full %>%
  ggplot(aes(x = rank)) +
  geom_point(aes(y = pval), color = "black", size = 0.5, shape=21) +
  geom_line(aes(y = bh_threshold, color=factor(alpha)), linewidth = 0.5) +
  labs(
    title = paste0("Benjamini-Hochberg FDR Control"),
    x = "P-value Rank",
    y = "P-value",
  ) +
  theme(panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "\U1D6FC", values=brewer.pal(9, "PuRd")[4:8])
# plot_BH
ggsave(paste0("results_fg_plot_BH.png"), plot_BH, width=6, height=3)


### Top N kickers
top_N = 10
df_topkickers = 
  df.EB.results.full %>%
  arrange(-mu.hat.i) %>%
  slice_head(n = top_N) %>%
  select(Player, mu.hat.i, mu.hat.MLE.i, N) %>%
  rename(mu_hat_EB = mu.hat.i, mu_hat_MLE = mu.hat.MLE.i)
df_topkickers

gt_topkickers = 
  gt(df_topkickers) %>%
  fmt_number("mu_hat_EB", decimals=3) %>%
  fmt_number("mu_hat_MLE", decimals=3) %>%
  cols_label(mu_hat_EB = html("Empirical Bayes <br> Estimate")) %>%
  cols_label(mu_hat_MLE = html("Observed Mean <br> (MLE)")) %>%
  tab_options(row_group.as_column = TRUE)  %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )  
# gt_topkickers
gtsave(gt_topkickers, "results_fg_plot_topkickers.png")

### Num significant kickers table
df_nSigkickers = 
  df_nsig.full %>%
  mutate(x = (1-alpha)*nsig) 
df_nSigkickers

gt_nSigkickers = 
  gt(df_nSigkickers) %>%
  cols_label(alpha = "\U1D6FC", nsig = "M", x = "(1-\U1D6FC)•M") %>%
  fmt_number("x", decimals=1) %>%
  tab_options(row_group.as_column = TRUE)  %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  ) 
# gt_nSigkickers
gtsave(gt_nSigkickers, paste0("results_fg_plot_BH_nSig.png"))
 


