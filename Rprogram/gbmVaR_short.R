gbmVaR_short <-function(v0, mu, sigma, p, t){
  VaR = v0 * exp(sigma * sqrt(t) * qnorm(p) + (mu - sigma^2/2 )*t ) - v0
}