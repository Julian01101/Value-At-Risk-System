#source("HistVaR.R")
source("UserDefinedInputs.R")

histVaR = vector()
histVaR2 = vector()
histES = vector()
histES2 = vector()

hrtn = exp(-diff(log(portValue), lag = horizon[1]*252))

n = length(portValue)

end = 0
for(i in 1:n){
  end = i + win[1]
  if(end > n){
    end = n
  }
  #print(quantile(s0*(1-hrtn[i:end]),VaRp[1]))
  if(i + win[1] > n ){
    histVaR = c(histVaR,NA)
    histES = c(histES,NA)
  }
  else{
    loss <-s0*(1-hrtn[i:end])
    histVaR = c(histVaR, quantile(s0*(1-hrtn[i:end]),VaRp[1],na.rm =T))
    histES = c(histES, mean(loss[loss > quantile(loss,ESp[1],na.rm = T)]))
  }
  
}

end = 0
for(i in 1:n){
  end = i + win[2]
  if(end > n){
    end = n
  }
  #print(quantile(s0*(1-hrtn[i:end]),VaRp[1]))
  if(i + win[2] > n ){
  
    histVaR2 = c(histVaR2,NA)
    histES2 = c(histES2,NA)
  }
  else{
    loss <-s0*(1-hrtn[i:end])
    histVaR2 = c(histVaR2, quantile(s0*(1-hrtn[i:end]),VaRp[1],na.rm =T))
    histES2 = c(histES2, mean(loss[loss > quantile(loss,ESp[1],na.rm = T)]))
  }
  
}
#plot(df$Date[1:3000],histVaR[1:3000],type = "l", ylab = "VaR",xlab = "Date")
#lines(df$Date[1:3000],gbmv[1:3000,2],col = 14)

