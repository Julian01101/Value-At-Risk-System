gbmES <- function(v0, mu, sigma, p, t){
  ES = v0*(1-exp(mu*t)/(1-p)* pnorm(qnorm(1-p)-sqrt(t)*sigma))
  return(ES)
} 