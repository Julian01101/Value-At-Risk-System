gbmES_short <- function(v0, mu, sigma, p, t){
  ES = v0*(exp(mu*t)/(1-p)* pnorm(sqrt(t)*sigma - qnorm(p)) - 1)
} 