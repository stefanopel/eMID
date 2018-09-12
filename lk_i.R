lk_i <- function(lai,i,nu,tau,phi,psi,ga,W,X0,X1,X2,Z,n,TT){

# compute log-likelihood
	phipsi = cbind(phi,psi,ga)
	lki = 0
	for(t in 1:TT){
		eta0 = X0[,,t]%*%nu+Z[,,t]%*%tau
		eta1 = eta0+X1[,,i,t]%*%lai
		for(j in (1:n)[-i]){
			if(all(!is.na(W[,i,j,t]))){
				eta = eta1+X2[,,j,t]%*%phipsi[j,]
				pijt = exp(eta)/sum(exp(eta))
				lki = lki + W[,i,j,t]%*%log(pijt)
			}
		}	
	}
# return output
	return(as.vector(lki))
	
}
