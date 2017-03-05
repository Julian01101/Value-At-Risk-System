gbmVaR <-function(v0, mu, sigma, p, t){
  VaR = v0 - v0 * exp(sigma * sqrt(t) * qnorm(1-p) + (mu - sigma^2/2 )*t )
  return(VaR)
}