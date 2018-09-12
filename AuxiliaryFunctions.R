# function to build W
W.build = function(Y){
  
  # Preliminaries
  n = nrow(Y)
  T = dim(Y)[3]
  
  # store memory for response matrix
  W = array(0,c(4,n,n,T))  

  # build response matrix
  for(t in 1:T){
    Yt = Y[,,t]		
    W[1,,,t] = (1-Yt)*t(1-Yt)
    W[2,,,t] = (1-Yt)*t(Yt)
    W[3,,,t] = Yt*t(1-Yt)
    W[4,,,t] = Yt*t(Yt)
  }
  return(W)
}

# function to build design matrix X1_ijt
X1.build = function(x){
  poly.order = length(x)
  X1 = matrix(0,4,poly.order*2)
  X1[2,(poly.order+1):(2*poly.order)] = x
  X1[3,1:poly.order] = x
  X1[4,] = x
  return(X1)
}

# function to build design matrix X2_ijt
X2.build = function(x){
  poly.order = length(x)
  X1 = matrix(0,4,poly.order*2)
  X1[3,(poly.order+1):(2*poly.order)] = x
  X1[2,1:poly.order] = x
  X1[4,] = x
  return(X1)
}

# function to build design matrix Z_t
Z.build = function(z){
  poly.order = length(z)
  Z = matrix(0,4,poly.order)
  Z[4,] = z
  return(Z)
}

# function to build p_ijt
p.build = function(l.i,l.j,tau,X1,X2,Z){
  lp = X1%*%l.i + X2%*%l.j + Z%*%tau
  p  = exp(lp-max(lp))
  p  = p/sum(p)
  return(p)
}
