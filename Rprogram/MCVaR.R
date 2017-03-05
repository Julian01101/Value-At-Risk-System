MCVaR <- function(shares,prices,mu,sigma,p,t,rho){
  la <- vector()
  a = matrix(shares,1,length(shares))
  
  s = matrix(prices,1,length(prices))
  
  
  #
  #sigma = sigmaStock_win[1,1:4]
  n = 1000
  mu = matrix(mu,1,length(mu))
  sigma = matrix(sigma,1,length(sigma))
  
  nstock = length(mu)
  
  as = matrix(a*s,1,length(shares))
  
  v0 = a%*%t(s)
  
  covm = r2cov(sigma,rho)
  
  B = matrix(rnorm(n*nstock), ncol=n)
  
  C = chol(covm)
  
  w = t(C)%*%B
  
  
  st = matrix(0, nrow = nstock, ncol = n)
  
  for(i in 1:nstock){
    for(j in 1:n){
      #print(c(i,j))
      st[i,j] = exp((mu[1,i]-sigma[1,i]^2)*t + sqrt(t)*w[i,j])
      
      
    }
  }
  
  vt = (a*s)%*%st

  v0 <- rep(v0,n)
  loss <- (v0 - vt)
  
  VaR <- quantile(loss,p)
  
  
  return(VaR)
}



