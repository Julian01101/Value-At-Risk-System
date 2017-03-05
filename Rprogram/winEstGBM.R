winEstGBM<- function(prices,windowLen){
  rtn   = -diff(log(prices))
  
  rtnsq = rtn^2
  
  w = rep(1/windowLen,windowLen)
  
  x2bar = rev(filter(rev(rtnsq),w,sides = 1))
  
  mubar = rev(filter(rev(rtn),w,sides = 1))
  
  var = x2bar - mubar^2
  zero = rep(0,length(var))
  sigmabar = sqrt(pmax(var,zero))
  
  sigma = sigmabar*sqrt(252)
  
  mu = mubar*252 + sigma^2/2
  df <- data.frame(cbind(rtn, mu, sigma,mubar,sigmabar))
  return(df)
}