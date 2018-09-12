dlk_i <- function(lai,i,nu,tau,phi,psi,ga,W,X0,X1,X2,Z,n,TT){

# compute log-likelihood
	phipsi = cbind(phi,psi,ga)
	dlki = rep(0,dim(X1)[2])
	for(t in 1:TT){
		eta0 = X0[,,t]%*%nu+Z[,,t]%*%tau
		eta1 = eta0+X1[,,i,t]%*%lai
		tmp = 0
		for(j in (i:n)[-i]){
			if(all(!is.na(W[,i,j,t]))){
				tot = sum(W[,i,j,t])
				eta = eta1+X2[,,j,t]%*%phipsi[j,]
				pijt = exp(eta)/sum(exp(eta))
				pijt[is.na(pijt)] = 1
				tmp = tmp+W[,i,j,t]-tot*pijt
			}
		}	
		dlki = dlki + t(X1[,,i,t])%*%tmp
	}
# return output
	return(as.vector(dlki))
	
}
