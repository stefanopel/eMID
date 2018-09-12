lk_overall <- function(th,phi,psi,ga,W,X0,X1,X2,Z,n,TT){

# compute log-likelihood
	np = length(th)/2
	nu = th[1:np]; tau = th[(np+1):(2*np)]
	phipsi = cbind(phi,psi,ga)
	lk = 0
	for(t in 1:TT){
		eta0 = X0[,,t]%*%nu+Z[,,t]%*%tau
		for(i in 1:(n-1)){
			eta1 = eta0+X1[,,i,t]%*%phipsi[i,]
			for(j in (i+1):n){
				if(all(!is.na(W[,i,j,t]))){
					eta = eta1+X2[,,j,t]%*%phipsi[j,]
					pijt = exp(eta)/sum(exp(eta))
					pijt[is.na(pijt)] = 1
					pijt[pijt==0] = 1e-10
					pijt = pijt/sum(pijt)
					lk = lk + W[,i,j,t]%*%log(pijt)
				}
			}
		}	
	}
	# return output
	return(as.vector(lk))
	
}
