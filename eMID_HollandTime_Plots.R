# Clean Environment and Console
rm(list = ls(all = TRUE))
cat("\014")

library(dplyr)
library(lubridate)
library(readr)
library(scales)
library(ggplot2)
library(gridExtra)
library(reshape)

setwd()

# ------------------------------------
# Model # 3
# Splines on each Q1 from 2007 to 2015
# ------------------------------------

# load output of eMID_HollandTime_Bootstrap.R
load("boostrap_splines.RData") 

# Initialize values
date = as.yearqtr(seq(as.Date("2006-01-01"), length = 40, by = "3 month"))
n = 58
TT = 40

actors = c("GR0002","IT0159","IT0160","IT0162","IT0165","IT0168","IT0169","IT0171","IT0173", # "IT0175",
           "IT0177","IT0183","IT0185","IT0189","IT0190","IT0191","IT0196","IT0197","IT0198","IT0199",
           "IT0202","IT0203","IT0205","IT0206","IT0208","IT0209","IT0213","IT0215","IT0217","IT0218",
           "IT0219","IT0220","IT0221","IT0222","IT0224","IT0225","IT0226","IT0229","IT0231","IT0232",
           "IT0237","IT0242","IT0243","IT0244","IT0253","IT0255","IT0256","IT0258","IT0260","IT0261",
           "IT0263","IT0264","IT0265","IT0269","IT0270","IT0273","IT0277","IT0278","IT0279")

B = dim(MUB)[1]; B # Bootstrap samples 

spline_knots = c(5,9,13,17,21,25,29,33,37)

col_points = rep("black", TT)
col_points[spline_knots] = "red"

mu  = est[["mu"]]
rho = est[["rho"]]
alpha = est[["Al"]][,1]
beta = est[["Be"]][,1]
gamma = est[["ga"]]

# ===================================
# MU: market tendency to connectivity
# BOOTSTRAPPED CONFIDENCE INTERVALS
# ===================================

df_MUB = cbind.data.frame(date = date, MUB = t(MUB))
dfm_MUB = melt(df_MUB, id.vars = c('date'))

df_mu = cbind.data.frame(date = date, mu = mu)
dfm_mu = melt(df_mu, id.vars = c('date'))

figure_2 <- 
  ggplot(dfm_MUB, aes(x = date, y = value, group = variable)) + 
  geom_line(size = 0.1, color = "gray70") +
  geom_line(data = dfm_mu, color = "black", size = 0.5) + # re-define and overwrite top layer inheritance
  geom_point(data = dfm_mu, size = 1.5, color = col_points) + 
  scale_x_yearqtr(breaks = seq(from = min(dfm_MUB$date), to = max(dfm_MUB$date), by = 0.25), format = "%YQ%q") +
  scale_y_continuous(breaks = pretty(dfm_MUB$value, n = 10)) +
  theme_light(base_size = 10) + 
  xlab(" ") +
  ylab(expression(mu[t])) +
  #ggtitle(" ") + 
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "plain", hjust = 0, vjust = 0), 
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 1),
        axis.title.x = element_text(size = 10, vjust = 0),
        axis.title.y = element_text(size = 10, vjust = 1.5),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 10), legend.direction = "horizontal")

print(figure_2)

ggsave(filename = "figure_2.jpeg", plot = figure_2)

# =====================================
# RHO: market tendency to reciprocation
# BOOTSTRAPPED CONFIDENCE INTERVALS
# =====================================

df_RHOB = cbind.data.frame(date = date, RHOB = t(RHOB))
dfm_RHOB = melt(df_RHOB, id.vars = c('date'))

df_rho = cbind.data.frame(date = date, rho = rho)
dfm_rho = melt(df_rho, id.vars = c('date'))

figure_3 <- 
  ggplot(dfm_RHOB, aes(x = date, y = value, group = variable)) + 
  geom_line(size = 0.1, color = "gray70") +
  geom_line(data = dfm_rho, color = "black", size = 0.5) + # re-define and overwrite top layer inheritance
  geom_point(data = dfm_rho, size = 1.5, colour = col_points) + 
  scale_x_yearqtr(breaks = seq(from = min(dfm_RHOB$date), to = max(dfm_RHOB$date), by = 0.25), format = "%YQ%q") +
  scale_y_continuous(breaks = pretty(dfm_RHOB$value, n = 10)) +
  theme_light(base_size = 10) + 
  xlab(" ") +
  ylab(expression(rho[t])) +
  #ggtitle(" ") + 
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "plain", hjust = 0, vjust = 0), 
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 1),
        axis.title.x = element_text(size = 10, vjust = 0),
        axis.title.y = element_text(size = 10, vjust = 1.5),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 10), legend.direction = "horizontal")

print(figure_3)

ggsave(filename = "figure_3.jpeg", plot = figure_3)

# =================================
# INDIVIDUAL TRAJECTORIES
# BOOTSTRAPPED CONFIDENCE INTERVALS
# =================================

# ------------------
# MOST BALANCED DYAD
# ------------------

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Consider the tuple composed of
# i = IT0258, j = IT0265
# This tuple is the most balanced one in termn of number of transactions
# IT0258 --> IT0265 #103
# IT0265 --> IT0258 #100
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

i = "IT0258"
j = "IT0265"

which(actors == i)
which(actors == j)

alpha_i = alpha[which(actors == i)]
alpha_j = alpha[which(actors == j)] 

beta_i = beta[which(actors == i)]
beta_j = beta[which(actors == j)]

gamma_i = gamma[which(actors == i)]
gamma_j = gamma[which(actors == j)]

# Which type of dyad is d_ij? It is a (1,1) type
# Compute UN-NORMALIZED probabilities
p.u_11 = exp(1 * (mu + rep(alpha_i, TT) + rep(beta_j, TT)) + 1 * (mu + rep(alpha_j, TT) + rep(beta_i, TT) + (rho + rep(gamma_i,TT) + rep(gamma_j,TT))))
# If d_ij were...
p.u_00 = rep(1, TT)
p.u_01 = exp(1 * (mu + rep(alpha_j, TT) + rep(beta_i, TT)))
p.u_10 = exp(1 * (mu + rep(alpha_i, TT) + rep(beta_j, TT)))
# Normalize the probability
p.ijt = p.u_11 / (p.u_11 + p.u_00 + p.u_01 + p.u_10)

# Initialize values for bootstrapped probabilities
alpha_B = lapply(lapply(ESTB, '[[', "Al"), "[", 1:58) # Alpha bootstrap
beta_B = lapply(lapply(ESTB, '[[', "Be"), "[", 1:58) # Beta bootstrap

alpha_i_B = unlist(lapply(alpha_B, '[[', which(actors == i)))
alpha_j_B = unlist(lapply(alpha_B, '[[', which(actors == j)))

beta_i_B = unlist(lapply(beta_B, '[[', which(actors == i)))
beta_j_B = unlist(lapply(beta_B, '[[', which(actors == j)))

gamma_i_B = GAB[ , which(actors == i)]
gamma_j_B = GAB[ , which(actors == j)]

# Bootstrapped probabilities
p.u_11_B = exp(1 * (MUB + rep(alpha_i_B,TT) + rep(beta_j_B,TT)) + 1 * (MUB + rep(alpha_j_B,TT) + rep(beta_i_B,TT)) + (rho + rep(gamma_i_B,TT) + rep(gamma_j_B,TT)))
p.u_00_B = matrix(1, B, TT) # Replace 17 con B
p.u_01_B = exp(1 * (MUB + rep(alpha_j_B,TT) + rep(beta_i_B, TT)))
p.u_10_B = exp(1 * (MUB + rep(alpha_i_B,TT) + rep(beta_j_B, TT)))

p.ijt_B = p.u_11_B / (p.u_11_B + p.u_00_B + p.u_01_B + p.u_10_B)

p.ijt_B_ci = apply(p.ijt_B, 2, quantile, probs = c(0.025,0.975),  na.rm = TRUE) 

# plot p.ijt_balance_boot
df_p.ijt_B = data.frame(date = date, p.ijt = p.ijt)
dfm_p.ijt_B = melt(df_p.ijt_B, id.vars = c('date'))
dfm_p.ijt_B$cilo = p.ijt_B_ci[1, ]
dfm_p.ijt_B$ciup = p.ijt_B_ci[2, ]

figure_6 <- 
  ggplot(dfm_p.ijt_B, aes(x = date, y = value, colour = variable)) + 
  # geom_errorbar(aes(ymin = cilo, ymax = ciup), width = 0.1, position = position_dodge(0)) +
  geom_ribbon(aes(date, ymin = cilo, ymax = ciup), alpha = 0.1) +
  geom_line(size = 0.5) +
  geom_point(size = 1.5) +
  scale_colour_manual(values = c("p.ijt" = "black"), 
                      labels = expression(p[ijt]))
  scale_x_yearqtr(breaks = seq(from = min(dfm_p.ijt_B$date), to = max(dfm_p.ijt_B$date), by = 0.25), format = "%YQ%q") +
  scale_y_continuous(breaks = pretty(dfm_p.ijt_B$ciup, n = 10)) +
  theme_light(base_size = 10) + 
  xlab("") +
  ylab(" ") +
  # ggtitle(" ") + 
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "plain", hjust = 0, vjust = 0),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 1),
        axis.title.x = element_text(size = 10, vjust = 0),
        axis.title.y = element_text(size = 10, vjust = 1.5),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(margin = margin(l = 8), hjust = 0, size = 10))
        
print(figure_6)

ggsave(filename = "figure_4.jpeg", plot = figure_6)

rm(df_p.ijt_B, dfm_p.ijt_B)

# =======================================
# Fix the SENDER, i=IT0258 
# Check how it is connected to the market
# =======================================

beta_jt.Alpha_00 = matrix(1, n-1, TT)

beta_jt.Alpha_01 = exp(t(replicate(n, mu)) + matrix(alpha_i, nrow = n, ncol = TT) + replicate(TT, beta))
beta_jt.Alpha_01 = beta_jt.Alpha_01[- which(actors == i), ]

beta_jt.Alpha_10 = exp(t(replicate(n, mu)) + matrix(beta_i, nrow = n, ncol = TT) + replicate(TT, alpha)) 
beta_jt.Alpha_10 = beta_jt.Alpha_10[- which(actors == i), ]

beta_jt.Alpha_11 = exp(t(replicate(n, mu)) + matrix(alpha_i, nrow = n, ncol = TT) + replicate(TT, beta) +
                         t(replicate(n, mu)) + matrix(beta_i, nrow = n, ncol = TT) + replicate(TT, alpha) +
                         t(replicate(n, rho)) + matrix(gamma_i, nrow = n, ncol = TT) + matrix(gamma_j, nrow = n, ncol = TT))
beta_jt.Alpha_11 = beta_jt.Alpha_11[- which(actors == i), ]

p_iJt_00 = colSums(beta_jt.Alpha_00 / (beta_jt.Alpha_00 + beta_jt.Alpha_01 + beta_jt.Alpha_10 + beta_jt.Alpha_11))
p_iJt_01 = colSums(beta_jt.Alpha_01 / (beta_jt.Alpha_00 + beta_jt.Alpha_01 + beta_jt.Alpha_10 + beta_jt.Alpha_11))
p_iJt_10 = colSums(beta_jt.Alpha_10 / (beta_jt.Alpha_00 + beta_jt.Alpha_01 + beta_jt.Alpha_10 + beta_jt.Alpha_11))
p_iJt_11 = colSums(beta_jt.Alpha_11 / (beta_jt.Alpha_00 + beta_jt.Alpha_01 + beta_jt.Alpha_10 + beta_jt.Alpha_11))

sump = p_iJt_11 + p_iJt_10 + p_iJt_01 + p_iJt_00
p_iJt_00 = p_iJt_00 / sump
p_iJt_01 = p_iJt_01 / sump
p_iJt_10 = p_iJt_10 / sump
p_iJt_11 = p_iJt_11 / sump

alpha_B = lapply(lapply(ESTB, '[[', "Al"), "[", 1:58) # Alpha bootstrap
beta_B = lapply(lapply(ESTB, '[[', "Be"), "[", 1:58) # Beta bootstrap

alpha_i_B = lapply(alpha_B, '[[', which(actors == i))
alpha_j_B = lapply(alpha_B, '[[', which(actors == j))

beta_i_B = lapply(beta_B, '[[', which(actors == i))
beta_j_B = lapply(beta_B, '[[', which(actors == j))

gamma_i_B = as.list(GAB[ , which(actors == i)])
gamma_j_B = as.list(GAB[ , which(actors == j)])

beta_jt.Alpha_00_B = lapply(1:B, matrix, data = 1, nrow = n-1, ncol = TT) 

beta_jt.Alpha_01_B = lapply(1:B, matrix, data = NA, nrow = n, ncol = TT)
for(b in 1:B)
{
  beta_jt.Alpha_01_B[[b]] = 
    exp(
      lapply(lapply(seq_len(nrow(MUB)), function(i) MUB[i,]), function(x) t(replicate(n, x)))[[b]] +
        lapply(alpha_i_B, function(x) t(replicate(n, rep(x, TT))))[[b]] + 
        lapply(beta_B, function(x) replicate(TT, x))[[b]]
    )
}
beta_jt.Alpha_01_B = lapply(beta_jt.Alpha_01_B, function(x) x[- which(actors == i), ])

beta_jt.Alpha_10_B = lapply(1:B, matrix, data = NA, nrow = n, ncol = TT)
for(b in 1:B)
{
  beta_jt.Alpha_10_B[[b]] =
    exp(
      lapply(lapply(seq_len(nrow(MUB)), function(i) MUB[i,]), function(x) t(replicate(n, x)))[[b]] +
        lapply(beta_i_B, function(x) t(replicate(n, rep(x, TT))))[[b]] + 
        lapply(alpha_B, function(x) replicate(TT, x))[[b]]
    )
}
beta_jt.Alpha_10_B = lapply(beta_jt.Alpha_10_B, function(x) x[- which(actors == i), ])

beta_jt.Alpha_11_B = lapply(1:B, matrix, data = NA, nrow = n, ncol = TT)
for(b in 1:B)
{
  beta_jt.Alpha_11_B[[b]] =
    exp(
      lapply(lapply(seq_len(nrow(MUB)), function(i) MUB[i,]), function(x) t(replicate(n, x)))[[b]] +
        lapply(alpha_i_B, function(x) t(replicate(n, rep(x, TT))))[[b]] + 
        lapply(beta_B, function(x) replicate(TT, x))[[b]] + 
        
        lapply(lapply(seq_len(nrow(MUB)), function(i) MUB[i,]), function(x) t(replicate(n, x)))[[b]] +
        lapply(beta_i_B, function(x) t(replicate(n, rep(x, TT))))[[b]] + 
        lapply(alpha_B, function(x) replicate(TT, x))[[b]] + 
        
        lapply(lapply(seq_len(nrow(RHOB)), function(i) RHOB[i,]), function(x) t(replicate(n, x)))[[b]] +
        lapply(gamma_i_B, function(x) t(replicate(n, rep(x, TT))))[[b]] + 
        lapply(gamma_j_B, function(x) t(replicate(n, rep(x, TT))))[[b]]
    )
}
beta_jt.Alpha_11_B = lapply(beta_jt.Alpha_11_B, function(x) x[- which(actors == i), ])

# Bootstrapped probabilities
p_iJt_00_B = lapply(1:B, matrix, data = NA, nrow = n-1, ncol = TT)
for(b in 1:B)
{
  p_iJt_00_B[[b]] = beta_jt.Alpha_00_B[[b]] / (beta_jt.Alpha_00_B[[b]] + beta_jt.Alpha_01_B[[b]] + beta_jt.Alpha_10_B[[b]] + beta_jt.Alpha_11_B[[b]])
}
p_iJt_00_B = lapply(p_iJt_00_B, function(x) colSums(x, na.rm = FALSE, dims = 1))

p_iJt_01_B = lapply(1:B, matrix, data = NA, nrow = n-1, ncol = TT)
for(b in 1:B)
{
  p_iJt_01_B[[b]] = beta_jt.Alpha_01_B[[b]] / (beta_jt.Alpha_00_B[[b]] + beta_jt.Alpha_01_B[[b]] + beta_jt.Alpha_10_B[[b]] + beta_jt.Alpha_11_B[[b]])
}
p_iJt_01_B = lapply(p_iJt_01_B, function(x) colSums(x, na.rm = FALSE, dims = 1))

p_iJt_10_B = lapply(1:B, matrix, data = NA, nrow = n-1, ncol = TT)
for(b in 1:B)
{
  p_iJt_10_B[[b]] = beta_jt.Alpha_10_B[[b]] / (beta_jt.Alpha_00_B[[b]] + beta_jt.Alpha_01_B[[b]] + beta_jt.Alpha_10_B[[b]] + beta_jt.Alpha_11_B[[b]])
}
p_iJt_10_B = lapply(p_iJt_10_B, function(x) colSums(x, na.rm = FALSE, dims = 1))

p_iJt_11_B = lapply(1:B, matrix, data = NA, nrow = n-1, ncol = TT)
for(b in 1:B)
{
  p_iJt_11_B[[b]] = beta_jt.Alpha_11_B[[b]] / (beta_jt.Alpha_00_B[[b]] + beta_jt.Alpha_01_B[[b]] + beta_jt.Alpha_10_B[[b]] + beta_jt.Alpha_11_B[[b]])
}
p_iJt_11_B = lapply(p_iJt_11_B, function(x) colSums(x, na.rm = FALSE, dims = 1))


sump_B = lapply(1:B, matrix, data = NA, nrow = 1, ncol = TT)
for(b in 1:B){
  sump_B[[b]] = p_iJt_00_B[[b]] + p_iJt_01_B[[b]] + p_iJt_10_B[[b]] + p_iJt_11_B[[b]]
}  

p_iJt_00_B = sapply(1:B, function(t) mapply("/",p_iJt_00_B[t], sump_B[t], SIMPLIFY = FALSE))
p_iJt_01_B = sapply(1:B, function(t) mapply("/",p_iJt_01_B[t], sump_B[t], SIMPLIFY = FALSE))
p_iJt_10_B = sapply(1:B, function(t) mapply("/",p_iJt_10_B[t], sump_B[t], SIMPLIFY = FALSE))
p_iJt_11_B = sapply(1:B, function(t) mapply("/",p_iJt_11_B[t], sump_B[t], SIMPLIFY = FALSE))

p_iJt_00_B = as.data.frame(do.call(rbind, p_iJt_00_B))
p_iJt_01_B = as.data.frame(do.call(rbind, p_iJt_01_B))
p_iJt_10_B = as.data.frame(do.call(rbind, p_iJt_10_B))
p_iJt_11_B = as.data.frame(do.call(rbind, p_iJt_11_B))

p_iJt_00_B_ci = sapply(p_iJt_00_B, function(i) quantile(i,c(0.025,0.975))) 
p_iJt_01_B_ci = sapply(p_iJt_01_B, function(i) quantile(i,c(0.025,0.975))) 
p_iJt_10_B_ci = sapply(p_iJt_10_B, function(i) quantile(i,c(0.025,0.975))) 
p_iJt_11_B_ci = sapply(p_iJt_11_B, function(i) quantile(i,c(0.025,0.975))) 

# ========================================
# Probability that the actor i=IT0258 is a
# 1. Not reciprocated SENDER and RECEIVER
# 2. reciprocated SENDER and RECEIVER
# ========================================

# p_iJt_00
df_p_iJt_00_B = data.frame(date = date, p_iJt_00 = p_iJt_00)
dfm_p_iJt_00_B = melt(df_p_iJt_00_B, id.vars = c('date'))

dfm_p_iJt_00_B$cilo = p_iJt_00_B_ci[1,]
dfm_p_iJt_00_B$ciup = p_iJt_00_B_ci[2,]

# head(kable(dfm_p_iJt_00_B))

# p_iJt_01
df_p_iJt_01_B = data.frame(date = date, p_iJt_01 = p_iJt_01)
dfm_p_iJt_01_B = melt(df_p_iJt_01_B, id.vars = c('date'))

dfm_p_iJt_01_B$cilo = p_iJt_01_B_ci[1,]
dfm_p_iJt_01_B$ciup = p_iJt_01_B_ci[2,]

# head(kable(dfm_p_iJt_01_B))

# p_iJt_10
df_p_iJt_10_B = data.frame(date = date, p_iJt_10 = p_iJt_10)
dfm_p_iJt_10_B = melt(df_p_iJt_10_B, id.vars = c('date'))

dfm_p_iJt_10_B$cilo = p_iJt_10_B_ci[1,]
dfm_p_iJt_10_B$ciup = p_iJt_10_B_ci[2,]

# head(kable(dfm_p_iJt_10_B))

# p_iJt_11
df_p_iJt_11_B = data.frame(date = date, p_iJt_11 = p_iJt_11)
dfm_p_iJt_11_B = melt(df_p_iJt_11_B, id.vars = c('date'))

dfm_p_iJt_11_B$cilo = p_iJt_11_B_ci[1,]
dfm_p_iJt_11_B$ciup = p_iJt_11_B_ci[2,]

# head(kable(dfm_p_iJt_11_B))

dfm_p_iJt_4_B = rbind.data.frame(dfm_p_iJt_00_B, dfm_p_iJt_01_B, dfm_p_iJt_10_B, dfm_p_iJt_11_B)

# head(kable(dfm_p_iJt_4_B))

figure_7 <- 
  ggplot(dfm_p_iJt_4_B, aes(x = date, y = value, colour = variable)) + 
  # geom_errorbar(aes(ymin = cilo, ymax = ciup), width = 0.1, position = position_dodge(0)) +
  geom_ribbon(aes(ymin = cilo, ymax = ciup), alpha = 0.1) +
  geom_line(size = 0.5) +
  geom_point(size = 1.5) +
  scale_color_discrete(labels = c(expression(p~bgroup("(", bold(D)[iJt] == list("(0,0)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")")),
                                  expression(p~bgroup("(", bold(D)[iJt] == list("(0,1)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")")),
                                  expression(p~bgroup("(", bold(D)[iJt] == list("(1,0)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")")),
                                  expression(p~bgroup("(", bold(D)[iJt] == list("(1,1)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")")))) +
  scale_x_yearqtr(breaks = seq(from = min(dfm_p_iJt_4_B$date), to = max(dfm_p_iJt_4_B$date), by = 0.25), format = "%YQ%q") +
  scale_y_continuous(breaks = pretty(dfm_p_iJt_4_B$value, n = 10)) +
  theme_light(base_size = 10) + 
  labs(x = " ", y = " ") +
  # ggtitle(" ") + 
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "plain", hjust = 0, vjust = 0), 
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 1),
        axis.title.x = element_text(size = 10, vjust = 0),
        axis.title.y = element_text(size = 10, vjust = 1.5),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(margin = margin(l = 8), hjust = 0, size = 10), legend.direction = "vertical")

print(figure_7)

ggsave(filename = "figure_7.jpeg", plot = figure_7)

# =============================================================
# Probability that actor i=IT0258 acts as a SENDER and RECEIVER 
# =============================================================

p_it = p_iJt_10 + p_iJt_11
p_jt = p_iJt_01 + p_iJt_11

p_it_B = p_iJt_10_B + p_iJt_11_B
p_jt_B = p_iJt_01_B + p_iJt_11_B

p_it_B_ci = sapply(p_it_B, function(i) quantile(i,c(0.025,0.975))) 
p_jt_B_ci = sapply(p_jt_B, function(i) quantile(i,c(0.025,0.975))) 

df_p_it_B = data.frame(date = date, p_it = p_it)
dfm_p_it_B = melt(df_p_it_B, id.vars = c('date'))

dfm_p_it_B$cilo = p_it_B_ci[1,]
dfm_p_it_B$ciup = p_it_B_ci[2,]

df_p_jt_B = data.frame(date = date, p_jt = p_jt)
dfm_p_jt_B = melt(df_p_jt_B, id.vars = c('date'))

dfm_p_jt_B$cilo = p_jt_B_ci[1,]
dfm_p_jt_B$ciup = p_jt_B_ci[2,]

dfm_p_it_jt_B = rbind.data.frame(dfm_p_it_B, dfm_p_jt_B)

# head(kable(dfm_p_it_jt_B))
                     
figure_9 <- 
  ggplot(dfm_p_it_jt_B, aes(x = date, y = value, colour = variable)) + 
  # geom_errorbar(aes(ymin = cilo, ymax = ciup), width = 0.1, position = position_dodge(0)) +
  geom_ribbon(aes(date, ymin = cilo, ymax = ciup), alpha = 0.1) +
  geom_line(size = 0.5) +
  geom_point(size = 1.5) +
  scale_color_discrete(labels = c(expression(p[it] == p~bgroup("(", bold(D)[iJt] == list("(1,0)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")") + p~bgroup("(", bold(D)[ijt] == list("(1,1)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")")),
                                  expression(p[it] == p~bgroup("(", bold(D)[iJt] == list("(0,1)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")") + p~bgroup("(", bold(D)[ijt] == list("(1,1)")~"|"~list(bold("\u03D5")[i],bold("\u03D5")[j]), ")")))) +
  scale_x_yearqtr(breaks = seq(from = min(dfm_p_it_jt_B$date), to = max(dfm_p_it_jt_B$date), by = 0.25), format = "%YQ%q") +
  scale_y_continuous(breaks = pretty(dfm_p_it_jt_B$ciup, n = 10)) +
  theme_light(base_size = 10) + 
  labs(x = " ", y = " ") +
  # ggtitle(" ") + 
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "plain", hjust = 0, vjust = 0), 
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 1),
        axis.title.x = element_text(size = 10, vjust = 0),
        axis.title.y = element_text(size = 10, vjust = 1.5),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(margin = margin(l = 8), hjust = 0, size = 10), legend.direction = "vertical")

print(figure_9)

ggsave(filename = "figure_9.jpeg", plot = figure_9)

# ==================
# alpha, beta, gamma
# SCATTERPLOTS
# ==================

library(GGally)

df_abg = cbind.data.frame(actors = actors, alpha = alpha, beta = beta, gamma = gamma)
dfm_abg = melt(df_abg, id.vars = c('actors'))

lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(size = 1.5, colour = "black") +
    geom_smooth(method = method, color = "blue", size = 0.5, ...)
  p
}

figure_6 <- 
  ggpairs(
    df_abg[, -1], 
    lower = list(continuous = wrap(lowerFn, method = "lm")),
    diag = list(continuous = wrap("barDiag", colour = "white")),
    upper = list(continuous = wrap("cor", size = 4))) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        axis.ticks = element_blank(), 
        panel.border = element_rect(linetype = "solid", colour = "gray31", fill = NA))

print(figure_6)

ggsave(filename = "figure_6.jpeg", plot = figure_6)