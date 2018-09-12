HollandEstimate = function(Y,poly.order,reltol=10^-10,nu0=NULL,phi0=NULL,psi0=NULL,
                                       ga0=NULL,tau0=NULL,adj=10^-4,IND=NULL,only.lk=FALSE){
 
	# Estimate latent trajectory model for longitudinal social network
	# GLOBAL TRAJECTORY
	# INDIVIDUAL MAIN EFFECTS (TIME CONSTANT)
	# INDIVIDUAL RECIPROCITY EFFECTS (TIME CONSTANT)
	#
	# INPUT:
	# Y = sequence of adjacency matrices
	# poly.order = order of the polynomial (or design matrix for splines and so on)
	# reltol = relative tollarance for convergence
	# phi0,psi0 = initial values of individual parameters (optional)
	# tau0 = initial values of reciprocity parameters (optional)
	# adj = adjustment for 0 cells
	# IND = matrix of data that are missing
	# only.lk = index of the time of which computing log-lik
	# 
	# OUPUT:
	# lk = final pairwise log-likelihood 
	# phi, psi = individual parameters of the model
	# tau = parameters for reciprocity
	# Al, Be = individual trajectories
	# mAl, mBe = average of individual trajectories
	# rho = trajectory of reciprocity  
	# X1,X2 = design matrices for Al,Be
	# Z = design matrix for tau
  
	# preliminaries
	check = FALSE # check derivatives
	n = dim(Y)[1]  # number of actors
	TT = dim(Y)[3]  # number of times
  
	# build polynomials
	if(is.matrix(poly.order)){
		x = z = t(poly.order)
	}else{
		x = rep(1,TT)
		if(poly.order>0) for(j in 1:poly.order) x = rbind(x,(((1:TT)-(TT+1)/2)/10)^j)
		z = x
	}
	np = nrow(x)		

	# build response matrices
	W = W.build(Y)
	W = pmax(W,adj)
	if(!is.null(IND)) for(j in 1:nrow(IND))
		W[,IND[j,1],IND[j,2],IND[j,3]] = W[,IND[j,2],IND[j,1],IND[j,3]] = NA
  
	# build design matrices X0, X1, and X2
	X0 = array(0,c(4,np,TT))
	X1 = X2 = array(0,c(4,3,n,TT))
  	for(t in 1:TT){
		X0[2,,t] = X0[3,,t] = x[,t]
		X0[4,,t] = 2*x[,t]
  		for(i in 1:n){
			X1[3,1,i,t] = X1[4,1,i,t] = X1[2,2,i,t] = X1[4,2,i,t] = X1[4,3,i,t] = 1
			X2[2,1,i,t] = X2[4,1,i,t] = X2[3,2,i,t] = X2[4,2,i,t] = X2[4,3,i,t] = 1
		}  
	}
  
	# build design matrix Z
	Z = array(0,c(4,np,TT))
	for(t in 1:TT) Z[4,,t] = z[,t]
	# to check consistency between individual and overall log-likelihood
	if(check){
		nu = rnorm(np)
		phi = rnorm(n,0,0.1)
		psi = rnorm(n,0,0.1)
		tau = rnorm(np)
		ga = rnorm(n,0,0.1)
		th = c(nu,tau)
		lk = lk_overall(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
		lk1 = 0
		for(i in 1:n){
			lai = c(phi[i],psi[i],ga[i])
			lk1 = lk1+lk_i(lai,i=i,nu,tau,phi=phi,psi=psi,ga=ga,W=W,
			                           X0=X0,X1=X1,X2=X2,Z=Z,n=n,TT=TT)
		}
		print(2*lk/lk1-1)
	}
  
	# to check derivative with respect to tau
	if(check){
		lk = lk_overall(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
		dlk = dlk_psi(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
		dlkn = rep(0,2*np)
		for(j in 1:(2*np)){
			th1 = th; th1[j] = th1[j]+10^-6
			lk1 = lk_overall(th1,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
			dlkn[j] = (lk1-lk)*10^6
		}
		print(cbind(dlk,dlkn,dlk/dlkn-1))
	}
  
	# to check derivative with respect to lai
	if(check){
		i = 1
		lai = c(phi[i],psi[i],ga[i])
		lki = lk_i(lai,i,nu,tau,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
		dlki = dlk_i(lai,i,nu,tau,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
		dlkin = rep(0,3)
		for(j in 1:3){
			lai1 = lai; lai1[j] = lai1[j]+10^-6
			lki1 = lk_i(lai1,i,nu,tau,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
			dlkin[j] = (lki1-lki)*10^6
		}
		print(cbind(dlki,dlkin,dlki/dlkin-1))
	}  
  
	### MAXIMIZATION ###
  
	# initial values
	if(is.null(nu0)) nu = rep(0,np) else nu=nu0
	if(is.null(phi0)) phi = rep(0,n) else phi=phi0
	if(is.null(psi0)) psi = rep(0,n) else psi=psi0
	if(is.null(ga0)) ga = rep(0,n) else ga=ga0
	if(is.null(tau0)) tau = rep(0,np) else tau=tau0
	th = c(nu,tau)

	# compute log-likelihood
	lk = lk_overall(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
	if(only.lk) return(lk)
	print(c(0,lk))
	it = 0; lko = lk
  
	# iterate until convergence
	while(abs(lk-lko)/abs(lko)>reltol | it==0){
		it = it+1
		lko = lk
		if(it==1) met="Nelder-Mead" else met = "BFGS"
    
		# maximize with respect to each lai
		for(i in 1:n){
			if(i/10==floor(i/10)) cat(i) else cat(".")
			if(i/50==floor(i/50)) cat("\n")
			lai = c(phi[i],psi[i],ga[i])
			lai = optim(lai,lk_i,gr=dlk_i,method=met,i=i,
			      nu=nu,tau=tau,phi=phi,psi=psi,ga=ga,W=W,X0=X0,X1=X1,X2=X2,Z=Z,n=n,TT=TT,
                  control=list(fnscale=-1,reltol=10^-12))$par
			phi[i] = lai[1]
			psi[i] = lai[2]
      if(it>1){
        lai = optim(lai,lk_i,gr=dlk_i,method="Nelder-Mead",i=i,
                    nu=nu,tau=tau,phi=phi,psi=psi,ga=ga,W=W,X0=X0,X1=X1,X2=X2,Z=Z,n=n,TT=TT,
                    control=list(fnscale=-1,reltol=10^-12))$par
        phi[i] = lai[1]
        psi[i] = lai[2]
        ga[i] = lai[3]
      }
		}
		cat("\n")

		# update nu and tau
		# th = c(nu,tau)
		# lk0 = lk_overall(th,phi=phi,psi=psi,ga=ga,W=W,X0=X0,X1=X1,X2=X2,Z=Z,n=n,TT=TT)
		# browser()
		nu = nu+(solve(x%*%t(x))%*%x%*%rep(1,TT))*(mean(phi)+mean(psi))
		phi = phi-mean(phi)
		psi = psi-mean(psi)
		tau = tau+2*+(solve(z%*%t(z))%*%z%*%rep(1,TT))*mean(ga)
		ga = ga-mean(ga)
		th = c(nu,tau)
		# lk1 = lk_overall(th,phi=phi,psi=psi,ga=ga,W=W,X0=X0,X1=X1,X2=X2,Z=Z,n=n,TT=TT)
		# print(c(lk0,lk1,lk0-lk1))
		
		th = c(nu,tau)
		th = optim(th,lk_overall,gr=dlk_psi,method=met,phi=phi,psi=psi,ga=ga,
	          W=W,X0=X0,X1=X1,X2=X2,Z=Z,n=n,TT=TT,
              control=list(fnscale=-1,trace=1,reltol=10^-12))$par
        nu = th[1:np]; tau = th[(np+1):(2*np)]
    
    # compute log-likelihood
    	lk = lk_overall(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT)
    	print(c(it,lk,lk-lko))

	}
  
	# output
	Al = phi%o%rep(1,TT)
	Be  = psi%o%rep(1,TT)
	mu = as.vector(nu%*%z)
	rho = as.vector(tau%*%z)
	out = list(lk=lk,mu=mu,Al=Al,Be=Be,rho=rho,phi=phi,psi=psi,ga=ga,nu=nu,tau=tau,X0=X0,X1=X1,X2=X2,Z=Z)
	return(out)

}

