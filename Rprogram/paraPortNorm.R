##############################################################################################
# Parametric Portfolio Normal
#
# Assumption: the underlying asset follows GBM and whole portfolio follows normal distribution
# run after paraPortGBM
#############################################################################################
source("UserDefinedInputs.R")
source("parametricVaR.R")
source("parametricES.R")
source("expEstGBM.R")
source("winEstGBM.R")
#source("DataProcessing.R")
# First, Calibrate GBM to each stock
dd = tradedays
# Window Set 1
winStock <- dfs[1:dd,1:nstock] #stores log returns,mu,sigma as whole data frame
expStock <- dfs[1:dd,1:nstock] #stores log returns,mu,sigma as whole data frame
muStock_win <- vector()  # stores mu as a matrix for the following calculation
sigmaStock_win <- vector() # stores sigma as a matrix for the following calculation
muStock_exp <- vector()
sigmaStock_exp <- vector()
rtns <- vector()

# Windows Set 2
winStock2 <- dfs[1:dd,1:nstock] 
expStock2 <- dfs[1:dd,1:nstock]
muStock_win2 <- vector()
sigmaStock_win2 <- vector()
muStock_exp2 <- vector()
sigmaStock_exp2 <- vector()

# Windows Set 1 
s1 = c("rtn","mu","sig")

for(j in 2:(nstock+1)){
  px_last <- as.numeric(data.matrix(data_example[,j]))
  
  df1 <- winEstGBM(px_last,win[1])[1:dd,1:3]
  #print(win[i])
  s2<- paste(colnames(data_example)[j],s1,sep = "_")
  colnames(df1) <- paste(s2,win_yr[1],sep = "_")
  winStock <- cbind(winStock,df1)
  rtns <- cbind(rtns,winEstGBM(px_last,win[1])[1:dd,1])
  muStock_win <- cbind(muStock_win,winEstGBM(px_last,win[1])[1:dd,2])
  sigmaStock_win <- cbind(sigmaStock_win,winEstGBM(px_last,win[1])[1:dd,3])
}

# Windows Set 2 
s1 = c("rtn","mu","sig")

for(j in 2:(nstock+1)){
  px_last <- as.numeric(data.matrix(data_example[,j]))
  
  df1 <- winEstGBM(px_last,win[2])[1:dd,1:3]
  #print(win[i])
  s2<- paste(colnames(data_example)[j],s1,sep = "_")
  colnames(df1) <- paste(s2,win_yr[2],sep = "_")
  winStock2 <- cbind(winStock2,df1)
  rtns <- cbind(rtns,winEstGBM(px_last,win[2])[1:dd,1])
  muStock_win2 <- cbind(muStock_win2,winEstGBM(px_last,win[2])[1:dd,2])
  sigmaStock_win2 <- cbind(sigmaStock_win2,winEstGBM(px_last,win[2])[1:dd,3])
}

# Exponentially weighted -stocks

# Weighted Set 1
s1 = c("rtn","mu","sig")

for(j in 2:(nstock+1)){
  px_last <- as.numeric(data.matrix(data_example[1:(dd+1),j]))
  
  df1 <- expEstGBM(px_last,lab[1])[1:dd,1:3]
  s2<- paste(colnames(data_example)[j],s1,sep = "_")
  colnames(df1) <- paste(s2,win_yr[1],sep = "_")
  expStock <- cbind(expStock[,1:nstock],df1)
  muStock_exp <- cbind(muStock_exp,expEstGBM(px_last,lab[1])[1:dd,2])
  sigmaStock_exp <- cbind(sigmaStock_exp,expEstGBM(px_last,lab[1])[1:dd,3])
}

# Weighted Set 2
s1 = c("rtn","mu","sig")

for(j in 2:(nstock+1)){
  px_last <- as.numeric(data.matrix(data_example[1:(dd+1),j]))
  
  df1 <- expEstGBM(px_last,lab[2])[1:dd,1:3]
  s2<- paste(colnames(data_example)[j],s1,sep = "_")
  colnames(df1) <- paste(s2,win_yr[2],sep = "_")
  expStock2 <- cbind(expStock2[,1:nstock],df1)
  muStock_exp2 <- cbind(muStock_exp2,expEstGBM(px_last,lab[2])[1:dd,2])
  sigmaStock_exp2 <- cbind(sigmaStock_exp2,expEstGBM(px_last,lab[2])[1:dd,3])
}



# Transform option shares to stocks shares by linearity assumption

clshare <- apply(dcall[2:dim(dcall)[2]],1,function(x){sum(x*shares_cl)})
csshare <- apply(dcall[2:dim(dcall)[2]],1,function(x){sum(x*shares_cs)})
plshare <- apply(dput[2:dim(dput)[2]],1,function(x){sum(x*shares_pl)})
psshare <- apply(dput[2:dim(dput)[2]],1,function(x){sum(x*shares_ps)})

stockshare <- as.vector(data.matrix(shares_sl - shares_ss))



#Windows Set 1

s0 = 10000
paraVaR = vector()
paraES = vector()

end = length(portValue)
for(i in 1:(end-100)){
   
  (rho <- cor(rtns[i:min(i+win-1,end),1:4]))
  sh = stockshare +clshare[i] + plshare[i] -csshare[i] - psshare[i]
  stockpx = as.vector(data.matrix(stock_price[i,2:(nstock+1)]))
  
  indexVal = sum(sum(sh*t(stockpx)))

  portPos = (sh/indexVal)*s0
  
  paraVaR = c(paraVaR, parametricVaR(portPos,stockpx,muStock_win[i,1:nstock],sigmaStock_win[i,1:nstock],rho,VaRp[1],horizon))
  paraES = c(paraES, parametricES(portPos,stockpx,muStock_win[i,1:nstock],sigmaStock_win[i,1:nstock],rho,ESp[1],horizon))
 
  
}

#Windows Set 2
paraVaR2 = vector()
paraES2 = vector()

end = length(portValue)
for(i in 1:(end-100)){
  
  (rho <- cor(rtns[i:min(i+win-1,end),1:4]))
  sh = stockshare+clshare[i]+plshare[i]-csshare[i]-psshare[i]
  stockpx = as.vector(data.matrix(stock_price[i,2:(nstock+1)]))
  
  indexVal = sum(sum(sh*t(stockpx)))
 
  portPos = (sh/indexVal)*s0
  
  paraVaR2 = c(paraVaR2, parametricVaR(portPos,stockpx,muStock_win2[i,1:nstock],sigmaStock_win2[i,1:nstock],rho,VaRp[1],horizon))
  paraES2 = c(paraES2, parametricES(portPos,stockpx,muStock_win2[i,1:nstock],sigmaStock_win2[i,1:nstock],rho,ESp[1],horizon))
  
}

#Weighted Set 1

s0 = 10000
paraVaR_e = vector()
paraES_e = vector()

end = length(portValue)
for(i in 1:(end-100)){
  
  (rho <- cor(rtns[i:min(i+win-1,end),1:4]))
  sh = stockshare +clshare[i] + plshare[i] -csshare[i] - psshare[i]
  stockpx = as.vector(data.matrix(stock_price[i,2:(nstock+1)]))
  
  indexVal = sum(sum(sh*t(stockpx)))

  portPos = (sh/indexVal)*s0
 
  paraVaR_e = c(paraVaR_e, parametricVaR(portPos,stockpx,muStock_exp[i,1:nstock],sigmaStock_exp[i,1:nstock],rho,VaRp[1],horizon))
  paraES_e = c(paraES_e, parametricES(portPos,stockpx,muStock_exp[i,1:nstock],sigmaStock_exp[i,1:nstock],rho,ESp[1],horizon))
 
  
}

#Weighted Set 2
paraVaR_e2 = vector()
paraES_e2 = vector()

end = length(portValue)
for(i in 1:(end-100)){
  
  (rho <- cor(rtns[i:min(i+win-1,end),1:4]))
  sh = stockshare +clshare[i] + plshare[i] -csshare[i] - psshare[i]
  stockpx = as.vector(data.matrix(stock_price[i,2:(nstock+1)]))
  
  indexVal = sum(sum(sh*t(stockpx)))

  portPos = (sh/indexVal)*s0

  paraVaR_e2 = c(paraVaR_e2, parametricVaR(portPos,stockpx,muStock_exp2[i,1:nstock],sigmaStock_exp2[i,1:nstock],rho,VaRp[1],horizon))
  paraES_e2 = c(paraES_e2, parametricES(portPos,stockpx,muStock_exp2[i,1:nstock],sigmaStock_exp2[i,1:nstock],rho,ESp[1],horizon))

}



  
