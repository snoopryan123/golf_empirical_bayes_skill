
source("A0_header.R")

###########################
### model specification ###
###########################

### Empirical Bayes Model:
# index (i,j): player i in hole j
# outcome X_{ij} for player i in hole j
#    putts over expected (putting) or strokes gained (driving, irons)
# X_{ij} ~ N(mu_i, sig^2_i)
#   mu_i ~ N(mu, tau^2)
# estimate putter skills {mu_i} for each putter i
# EB estimators: 
#     V_hat_i = 1 / [N_i/sig_hat^2_i + 1/tau_hat^2]
#     mu_hat_i = V_hat_i • [(sum_j X_{ij})/sig_hat^2_i + mu_hat/tau_hat^2] 
#     posterior mu_i | {X_{ij}} ~ N(mu_hat_i, V_hat_i)

#################
### read data ###
#################

df0 = read_csv("data_holeByHole_byGrp.csv", show_col_types = F)
df0$stroke_grp = factor(df0$stroke_grp, levels=stroke_grp_levels)
df0

metrics_list = list(
  "Driving" = "Stroked Gained (higher is better)",
  "Approaching" = "Strokes Under Expected (higher is better)",
  "Putting" = "Putts Under Expected (higher is better)"
)

######################################
### Empirical Bayes Skill Function ###
######################################

fit_params.EB.mle <- function(epsilon=1e-4, df0) {
  ### first, fit the MLE hyperparams mu.hat, tau.sq.hat, sig.sq.i.hat
  ### initialize
  iter = 1
  df = df0 %>% select(Player_num, X)
  mu.hat = df %>% group_by(Player_num) %>% reframe(mu = mean(X)) %>% reframe(mu = mean(mu))
  full.var = var(df$X)
  params.df = df %>% 
    group_by(Player_num) %>% 
    reframe(sig.sq.i.hat = var(X)) %>% 
    mutate(sig.sq.i.hat = median(sig.sq.i.hat))
  params.df$mu.hat = mu.hat$mu
  params.df$tau.sq.hat = full.var - mean(params.df$sig.sq.i.hat)
  params.df
  train.df = df %>% left_join(params.df) 
  train.df

  ### iteratively solve for MLE mu.hat, tau.sq.hat, sig.sq.i.hat
  while (TRUE) {
    print(paste0("running iteration ", iter))
    ### save previous values
    train.df.prev = train.df
    ### solve for mu.hat
    mu.hat = train.df %>%
      reframe(
        t1 = sum(X / (sig.sq.i.hat + tau.sq.hat)),
        t2 = sum(1 / (sig.sq.i.hat + tau.sq.hat)),
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
    # tau.sq.hat = uniroot(solve.for.tau.sq, c(0.0001,0.02))
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

for (stroke_category in unique(df0$stroke_grp)) {
  print(stroke_category)
  
  ### dataframe
  df_s = df0 %>% filter(stroke_grp == stroke_category) %>% rename(X = SUE, N = nHoles_grp)
  
  ### fit the empirical bayes parameters
  df_EB_fit = fit_params.EB.mle(df0 = df_s)
  df_EB_fit
  
  ### cleaned Empirical Bayes results dataframe
  df.EB.results = 
    df_EB_fit %>%
    select(Player_num, mu.hat.i, V.hat.i) %>%
    left_join(
      df_s %>% distinct(Player_num, Player, N)
    ) %>%
    left_join(
      df_s %>% group_by(Player,Player_num,N) %>% reframe(mu.hat.MLE.i = mean(X))
    ) %>%
    relocate(Player, .after=Player_num) %>%
    mutate(
      p_val = 2*pnorm(-abs(mu.hat.i)/sqrt(V.hat.i))
    ) %>%
    arrange(p_val) 
  df.EB.results$stroke_category = stroke_category
  df.EB.results
  df.EB.results.full = bind_rows(df.EB.results.full, df.EB.results)
  
  ### Multiple Testing: Benjamini-Hochberg (BH) to control the False Discovery Rate (FDR)
  nsig_golfers = c()
  df_pvals_BH = tibble()
  for (alpha in alphas) {
    ### Multiple Testing: Benjamini-Hochberg (BH) to control the False Discovery Rate (FDR)
    p_vals = df.EB.results$p_val
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
    df_pvals_BH$stroke_category = stroke_category
    df_pvals_BH.full = bind_rows(df_pvals_BH.full, df_pvals_BH)
  }
  df_nsig = tibble(alpha = alphas, nsig = nsig_golfers, stroke_category = stroke_category)
  df_nsig
  df_nsig.full = bind_rows(df_nsig.full, df_nsig)

}
df.EB.results.full$stroke_category = factor(df.EB.results.full$stroke_category, levels = stroke_grp_levels)
df_pvals_BH.full$stroke_category = factor(df_pvals_BH.full$stroke_category, levels = stroke_grp_levels)
df_nsig.full$stroke_category = factor(df_nsig.full$stroke_category, levels = stroke_grp_levels)

################
### Plotting ###
################

###
df.EB.results.full
df_pvals_BH.full
df_nsig.full

### visualize
num_golfers = nrow(df.EB.results)
plot_EB_estimates = 
  df.EB.results.full %>%
  ggplot(aes(x = mu.hat.i)) +
  facet_wrap(~ stroke_category) + 
  geom_histogram(fill="black") +
  geom_vline(xintercept=0,linetype="dashed",color="gray50",linewidth=1.5) +
  xlab(TeX("Empirical Bayes estimate $\\hat{\\mu}^{(EB)}_i$")) +
  # scale_x_continuous(breaks=seq(-1,1,by=0.05)) +
  ylab("Count") +
  labs(title = paste0("Distribution of Estimated Golfer Skills"))
# plot_EB_estimates
ggsave(paste0("results_plot_EB.png"), plot_EB_estimates, width=8, height=3)

### visualize: how much shrinkage?
plot_EB_shrinkage = 
  df.EB.results.full  %>%
  ggplot(aes(x=mu.hat.MLE.i, y=mu.hat.i, color=N, size=N)) +
  facet_wrap(~ stroke_category) + 
  xlab(TeX("Observed Mean $\\hat{\\mu}^{(MLE)}_i$")) +
  ylab(TeX("Empirical Bayes estimate $\\hat{\\mu}^{(EB)}_i$")) +
  geom_abline(intercept=0, slope=1, linewidth=1, linetype="dashed", color="gray60") +
  geom_point(alpha=0.6) +
  labs(title = paste0("Visualizing Shrinkage in Estimating Golfer Skills"))
# plot_EB_shrinkage
ggsave(paste0("results_plot_EB_shrinkage.png"), plot_EB_shrinkage, width=10, height=4)

### plot Benjamini Hochberg
max_rank = (df_pvals_BH.full %>% group_by(stroke_category) %>% filter(pval <= max(alphas)) %>% reframe(m=max(rank)) %>% reframe(m=max(m)))$m
max_rank
plot_BH0 = 
  df_pvals_BH.full %>%
  ggplot(aes(x = rank)) +
  facet_wrap(~ stroke_category) + 
  geom_point(aes(y = pval), color = "black", size = 0.5, shape=21) +
  geom_line(aes(y = bh_threshold, color=factor(alpha)), linewidth = 0.5) +
  labs(
    title = paste0("Benjamini-Hochberg FDR Control"),
    x = "P-value Rank",
    y = "P-value",
  ) +
  ylim(c(0, max(alphas))) +
  xlim(c(0,max_rank)) +
  scale_color_manual(name = "\U1D6FC", values=brewer.pal(9, "PuRd")[4:8])
# plot_BH0
ggsave(paste0("results_plot_BH0.png"), plot_BH0, width=8, height=3)

### plot Benjamini Hochberg
max_ranks = df_pvals_BH.full %>% filter(alpha == max(alphas), rank > 25) %>% group_by(stroke_category) %>% mutate(diff = abs(pval - bh_threshold)) %>% arrange(diff) %>% slice_head() %>% ungroup() %>% select(stroke_category, rank) %>% rename(max_rank = rank)
max_ranks$max_rank = max_ranks$max_rank + 5
max_ranks
plot_BH = 
  df_pvals_BH.full %>%
  left_join(max_ranks) %>%
  filter(pval <= max(alphas), rank <= max_rank) %>%
  ggplot(aes(x = rank)) +
  facet_wrap(~ stroke_category, scales="free_x") + 
  geom_point(aes(y = pval), color = "black", size = 0.5, shape=21) +
  geom_line(aes(y = bh_threshold, color=factor(alpha)), linewidth = 0.5) +
  labs(
    title = paste0("Benjamini-Hochberg FDR Control"),
    x = "P-value Rank",
    y = "P-value",
  ) +
  ylim(c(0, max(alphas))) +
  scale_color_manual(name = "\U1D6FC", values=brewer.pal(9, "PuRd")[4:8])
# plot_BH
ggsave(paste0("results_plot_BH.png"), plot_BH, width=8, height=3)

### Top 5 Golfers
top_N = 5
df_topGolfers = 
  df.EB.results.full %>%
  arrange(stroke_category, -mu.hat.i) %>%
  group_by(stroke_category) %>%
  slice_head(n = top_N) %>%
  select(stroke_category, Player, mu.hat.i, N) %>%
  rename(Stroke = stroke_category, mu_hat = mu.hat.i)
df_topGolfers

gt_topGolfers = 
  gt(df_topGolfers) %>%
  fmt_number("mu_hat", decimals=3) %>%
  # cols_label(mu_hat = "\U00B5") %>%
  cols_label(mu_hat = html("Estimated \U00B5 via <br> Empirical Bayes")) %>%
  tab_options(row_group.as_column = TRUE)  %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )  
# gt_topGolfers
gtsave(gt_topGolfers, "results_plot_topGolfers.png")

### Num significant golfers table
df_nSigGolfers = 
  df_nsig.full %>%
  relocate(stroke_category, .before = alpha) %>%
  rename(Stroke = stroke_category) %>%
  mutate(x = (1-alpha)*nsig) %>%
  group_by(Stroke)
df_nSigGolfers

gt_nSigGolfers = 
  gt(df_nSigGolfers) %>%
  cols_label(alpha = "\U1D6FC", nsig = "M", x = "(1-\U1D6FC)•M") %>%
  tab_options(row_group.as_column = TRUE)  %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  ) 
# gt_nSigGolfers
gtsave(gt_nSigGolfers, paste0("results_plot_BH_nSig.png"))

