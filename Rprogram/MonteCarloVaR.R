source("MCVaR.R")
source("MCES.R")
source("UserDefinedInputs.R")

# Window Set 1
s0 = 10000
mcVaR = vector()
mcES = vector()

end = length(portValue)
for(i in 1:(end-100)){
  
  (rho <- cor(rtns[i:min(i+win[1]-1,end),1:4]))
  sh = stockshare+clshare[i]+plshare[i]-csshare[i]-psshare[i]
  stockpx = as.vector(data.matrix(stock_price[i,2:(nstock+1)]))
  
  indexVal = sum(sum(sh*t(stockpx)))
  #print(indexVal)
  portPos = (sh/indexVal)*s0
  # Inputs: shares,prices,mu,sigma,rho,p,t
  #print(portPos)
  
  mcVaR = c(mcVaR, MCVaR(portPos,stockpx,muStock_win[i,1:nstock],sigmaStock_win[i,1:nstock],VaRp[1],horizon,rho))
  mcES = c(mcES, MCES(portPos,stockpx,muStock_win[i,1:nstock],sigmaStock_win[i,1:nstock],ESp[1],horizon,rho))
  #print(paraVaR)
  #shares,prices,mu,sigma,p,t,rho
}

# Window Set 2
s0 = 10000
mcVaR2 = vector()
mcES2 = vector()

end = length(portValue)
for(i in 1:(end-100)){
  
  (rho <- cor(rtns[i:min(i+win[1]-1,end),1:4]))
  sh = stockshare+clshare[i]+ plshare[i]-csshare[i]-psshare[i]
  stockpx = as.vector(data.matrix(stock_price[i,2:(nstock+1)]))
  
  indexVal = sum(sum(sh*t(stockpx)))
  #print(indexVal)
  portPos = (sh/indexVal)*s0
  # Inputs: shares,prices,mu,sigma,rho,p,t
  #print(portPos)
  
  mcVaR2= c(mcVaR2, MCVaR(portPos,stockpx,muStock_win2[i,1:nstock],sigmaStock_win2[i,1:nstock],VaRp[1],horizon,rho))
  mcES2 = c(mcES2, MCES(portPos,stockpx,muStock_win2[i,1:nstock],sigmaStock_win2[i,1:nstock],ESp[1],horizon,rho))
  #print(paraVaR)
  #shares,prices,mu,sigma,p,t,rho
}


#plot(df$Date[1:2500],histES2[1:2500],type = "l",col = 3, ylab = "VaR",xlab = "Date",ylim = c(0,1800))
#lines(df$Date[1:2500],mcVaR2[1:2500],col = 5)
#lines(df$Date[1:2500],paraVaR2[1:2500],col = 1)
#lines(df$Date[1:2500],gbmv[1:2500,3],col = 6)


