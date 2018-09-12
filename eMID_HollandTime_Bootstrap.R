# Clean Environment and Console
rm(list = ls(all = TRUE))
cat("\014")

# setwd()

# load data
load("estimation_splines.RData")
attach(est)

# load packages and auxiliary functions 
require(splines)

source("AuxiliaryFunctions.R")
source("lk_overall.R")
source("lk_i.R")
source("dlk_i.R")
source("dlk_psi.R")
source("HollandEstimate.R")

# setup
n          = dim(Yb)[1]
TT         = dim(Yb)[3]
knots      = c(5,9,13,17,21,25,29,33,37)
x1         = bs(1:TT,knots = knots,intercept = TRUE)
file_name  = "boostrap_splines.RData"

# setup
B = 250 # number of bootstrap repetition
reltol = 10^-10

# BOOTSTRAP
set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
MUB = RHOB = PHIB = PSIB = GAB = NULL
ESTB = vector("list",B)
phipsi = cbind(phi,psi,ga)
for(b in 1:B){
  print(b)
  # generate bootstrap samples
  Yb = array(0,c(n,n,TT))
  for(t in 1:TT){
    eta0 = X0[,,t]%*%nu+Z[,,t]%*%tau
    for(i in 1:(n-1)){
      eta1 = eta0+X1[,,i,t]%*%phipsi[i,]
      for(j in (i+1):n){
        eta = eta1+X2[,,j,t]%*%phipsi[j,]
        pijt = exp(eta)/sum(exp(eta))
        r = which(rmultinom(1,1,pijt)==1)
        if(r==2) Yb[j,i,t] = 1
        if(r==3) Yb[i,j,t] = 1
        if(r==4) Yb[i,j,t] = Yb[j,i,t] = 1
      }
    }	
  }
  # estimate
	ESTB[[b]] = HollandEstimate(Yb,x1,reltol=reltol,nu=nu,phi0=phi,psi0=psi,ga0=ga,tau=tau,adj=adj)
	MUB = rbind(MUB,ESTB[[b]]$mu)
	RHOB = rbind(RHOB,ESTB[[b]]$rho)
	PHIB = rbind(PHIB,ESTB[[b]]$phi)
	PSIB = rbind(PSIB,ESTB[[b]]$psi)
	GAB = rbind(GAB,ESTB[[b]]$ga)
	save.image(file_name)

# # plot	
# 	par(mfrow=c(2,1))
# 	plot(mu,type="l",main="mu",ylim=c(min(min(MUB),min(mu)),max(MUB,max(mu))),lwd=2)
# 	for(b1 in 1:b) lines(MUB[b1,],col=8)
# 	lines(mu,type="l",lwd=2)
# 	plot(rho,type="l",main="rho",ylim=c(min(min(RHOB),min(rho)),max(max(RHOB),max(rho))),lwd=2)
# 	for(b1 in 1:b) lines(RHOB[b1,],col=8)
# 	lines(rho,type="l",lwd=2)
# 
}
