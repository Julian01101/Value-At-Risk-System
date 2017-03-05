HistVaR <- function(prices,winLen,s0,VaR,horizon){
  histVaR = vector()
  hrtn = exp(-diff(log(prices), lag = horizon))
  
  n = length(prices)
  
  while(length(hrtn) < n){
    rtn = c(rtn,0)
  }
 
  end = 0
  for(i in 1:n){
    end = i + winLen
    if(end > n){
      end = n
    }
    print(quantile(s0*(1-hrtn[i:end]),1-VaR))
    histVaR = c(histVaR, quantile(s0*(1-hrtn[i:end]),1-VaR))
  }
  
  return(histVaR)
}