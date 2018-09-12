dlk_psi <- function(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT){

# compute probabilities
	np = length(th)/2
	nu = th[1:np]; tau = th[(np+1):(2*np)]
	phipsi = cbind(phi,psi,ga)
	dlk = rep(0,dim(X0)[2]+dim(Z)[2])
	for(t in 1:TT){
		eta0 = X0[,,t]%*%nu+Z[,,t]%*%tau
		tmp = 0
		for(i in 1:(n-1)){
			eta1 = eta0+X1[,,i,t]%*%phipsi[i,]
			lai = phipsi[i,]
			for(j in (i+1):n){
				if(all(!is.na(W[,i,j,t]))){
					laj = phipsi[j,]
					tot = sum(W[,i,j,t])
					eta = eta1+X2[,,j,t]%*%phipsi[j,]
					pijt = exp(eta)/sum(exp(eta))
					pijt[is.na(pijt)] = 1
					tmp = tmp+W[,i,j,t]-tot*pijt
				}
			}
		}	
		dlk = dlk+t(cbind(X0[,,t],Z[,,t]))%*%tmp
	}
# return
	return(as.vector(dlk))
	
}
