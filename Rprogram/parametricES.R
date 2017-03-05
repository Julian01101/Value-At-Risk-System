parametricES <- function(shares,prices,mu,sigma,rho,p,t){
  # share - a vector stores all shares of the underlying asset
  # prices - a vector of stock prices at t = 0
  # mu - a vector of all sigma
  # sigma - a vector of all sigma
  # rho - correlation matrix
  #shares = portPos
  #prices = stockpx
  #
  a = matrix(shares,1,length(shares))
  
  s = matrix(prices,1,length(prices))
  #mu = muStock_win[1,1:4]
  #sigma = sigmaStock_win[1,1:4]
  
  mu = matrix(mu,1,length(mu))
  sigma = matrix(sigma,1,length(sigma))
  
  nstock = length(mu)
  
  as = matrix(a*s,1,length(shares))
  
  v0 = a%*%t(s)
  #print(v0)
  covm = r2cov(sigma,rho)
  
  evt = as%*%t(exp(mu * t))# E[V_t] 
  #print(evt)
  evt2 = exp(mu*t)%*%(exp(covm * t)*(t(as)%*%as))%*%t(exp(mu*t))
  
  varvt = evt2 - evt^2 # var[V_t]
  sdvt = sqrt(varvt)
  ES = v0 - evt + sdvt * dnorm(qnorm(p))/(1-p)
  return(ES)
}