deltaPut <- function(s0,k,T,sigma,r){
  d1 = (log(s0/k) + (r+sigma^2/2)*T)/(sigma*sqrt(T))
  return(-pnorm(-d1))
}