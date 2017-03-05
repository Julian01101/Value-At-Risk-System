bsCall <- function(s0,k,T,sigma,r){
  d1 = (log(s0/k) + (r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 = (log(s0/k) + (r-sigma^2/2)*T)/(sigma*sqrt(T))
  call = s0*pnorm(d1) - k*exp(-r*T)*pnorm(d2)  
  return(call)
}