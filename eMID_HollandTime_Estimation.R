# Initial model estimate with 
# 1. global trajectories: mu(t) and rho(t) 
# 2. individual parameters: alpha_i, beta_i, gamma_i   
# 4. SPLINE in each year

rm(list = ls(all = TRUE))
cat("\014")  

# setwd()

# load packages and auxiliary functions 
require(splines)

source("AuxiliaryFunctions.R")
source("lk_overall.R")
source("lk_i.R")
source("dlk_i.R")
source("dlk_psi.R")
source("HollandEstimate.R")

# load data
load("pseudodata.RData")
# Yb: bootstrapped pseudodata

# fix framework 
TT         = dim(Yb)[3]
knots      = c(5,9,13,17,21,25,29,33,37)
X          = bs(1:TT, knots = knots,intercept=TRUE)
nknots     = length(knots)       
t.scale    = 1       # scale of t to avoid Hessian numerical singularity 
adj        = 10^-06  # adjustment for 0 cells
file_name  = "estimation_splines.RData"

# Check Y for all zeros
sum1 = apply(Yb,1,sum)
if(any(sum1 == 0)){
  ind1 = which(sum1 == 0)
  Yb = Yb[-ind1,-ind1,]
  print("remove units")
}
sum2 = apply(Yb,2,sum); print(any(sum2 == 0))
if(any(sum2 == 0)){
  ind2 = which(sum2 == 0)
  Yb = Yb[-ind2,-ind2,]
  print("remove units")
}

# Estimation
t0  = proc.time()
est = HollandEstimate(Yb, X, adj = adj)
t1  = proc.time()
save.image(file_name)

# plot global parameters
# par(mfrow = c(2,1))
# plot(est$mu, type  = "l", xlab = "time",ylab = "connectedness")
# plot(est$rho,type = "l", xlab = "time",ylab = "reciprocity")

