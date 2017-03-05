bsPut <- function(s0,k,T,sigma,r){
  d1 = (log(s0/k) + (r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 = (log(s0/k) + (r-sigma^2/2)*T)/(sigma*sqrt(T))
  put = k*exp(-r*T)*pnorm(-d2) - s0*pnorm(-d1)
  return(put)
}