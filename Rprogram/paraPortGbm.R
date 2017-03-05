##############################################################################################
# Parametric Portfolio Gbm
#
# Assumption: the whole portfolio follows GBM
#############################################################################################
source("UserDefinedInputs.R")
# Caculate Portfolio values
# Windows - stocks portfolio
source("DataProcessing.R")
source("winEstGBM.R")
source("expEstGBM.R")
source("gbmVaR.R")
source("gbmES.R")

# shares of each stock  
shares_sl
shares_ss
shares_cl
shares_cs
shares_pl
shares_ps

stock_price <- data_example[1:tradedays,1:(nstock+1)]
# Transform option shares to stocks shares by linearity assumption
lnstocks <- apply(stock_price[2:dim(stock_price)[2]],1,function(x){sum(x*shares_sl)})
snstocks <- apply(stock_price[2:dim(stock_price)[2]],1,function(x){sum(x*shares_ss)})
lncalls <- apply(call_price[2:dim(call_price)[2]],1,function(x){sum(x*shares_cl)})
sncalls <- apply(call_price[2:dim(call_price)[2]],1,function(x){sum(x*shares_cs)})
lnputs <- apply(put_price[2:dim(put_price)[2]],1,function(x){sum(x*shares_pl)})
snputs <- apply(put_price[2:dim(put_price)[2]],1,function(x){sum(x*shares_ps)})

portfolio <-cbind(lnstocks,lncalls,lnputs,snstocks,sncalls,snputs)
portValue <- apply(portfolio,1,function(x){sum(x[1:3])-sum(x[4:6])})


win_yr = c("2yr","3yr","4yr","5yr","6yr","7yr","8yr","9yr","10yr")
expo_yr = c("2yr","3yr","4yr","5yr","6yr","7yr","8yr","9yr","10yr")

s1 = c("rtn","mu","sig")

port <- data.frame(df$Date)
portValue2 = portValue 
dd = tradedays

# window
for(i in 1:length(win)){
  
  df1 <- winEstGBM(portValue2,win[i])[1:dd,1:3]
  
  s2<- paste("win",s1,sep = "_")
  colnames(df1) <- paste(s2,win_yr[i],sep = "_")
 
  
  port = cbind(port,df1)
  s1 = c("rtn","mu","sig")
}

# exponential
s1 = c("rtn","mu","sig")

for(i in 1:length(win)){
  
  df2 <- expEstGBM(portValue2,lab[i])[1:dd,1:3]
 
  
  s2<- paste("exp",s1,sep = "_")
  colnames(df2) <- paste(s2,expo_yr[i],sep = "_")

  port = cbind(port,df2)
  s1 = c("rtn","mu","sig")
}

## Fit Gbm 

# gbmVaR and gbmES


port <- port[1:(dim(port)[1]-1),]
varlab1 <- vector()
varlab2 <- vector()
eslab1 <- vector()
eslab2 <- vector()
# Generate list of var names
for(i in 1:length(horizon)){
  for(j in 1:length(VaRp)){
    varlab1 <- c(varlab1,paste("GbmVaR Win ",paste(as.character(VaRp[j]),hor[i],sep=" "),sep = ""))
    varlab2 <-  c(varlab2,paste("GbmVaR Exp ",paste(as.character(VaRp[j]),hor[i],sep=" "),sep = ""))
    eslab1 <-  c(eslab1,paste("GbmES win ",paste(as.character(ESp[j]),hor[i],sep=" "),sep = ""))
    eslab2 <-  c(eslab2,paste("GbmES Exp ",paste(as.character(ESp[j]),hor[i],sep=" "),sep = ""))
  }
}

# gbmVaR, totally 60, windows
indx = 3

gbmv <- data.frame(port$df.Date)
colnames(gbmv) <- "Date"
s1 <- "VaR"
for(i in 1:length(horizon)){
  for(j in 1:length(VaRp)){
    for(k in 0:(length(win)-1)){
      
      mylist <- data.frame(mapply(gbmVaR,s0,port[,(indx+3*k)],port[,(indx+1+3*k)],VaRp[j],horizon[i]))
      #print(dim(mylist))
      s1 <- paste(s1,paste(win_yr[k+1],paste(as.character(VaRp[j]),hor[i],sep="_"),sep="_"),sep = "_")
      colnames(mylist) <- s1
      
      gbmv = cbind(gbmv,mylist)
      s1 <- "VaR"
    }
  }
}

# gbmES, totally 60, windows 
gbme <- data.frame(port$df.Date)
colnames(gbme) <- "Date"
s1 <- "ES"
for(i in 1:length(horizon)){
  for(j in 1:length(ESp)){
    for(k in 0:(length(win)-1)){
      mylist <- data.frame(mapply(gbmES,s0,port[,indx+3*k],port[,indx+1+3*k],ESp[j],horizon[i]))
      s1 <- paste(s1,paste(win_yr[k+1],paste(as.character(ESp[j]),hor[i],sep="_"),sep="_"),sep = "_")
      colnames(mylist) <- s1
      
      gbme = cbind(gbme,mylist)
      s1 <- "ES"
    }
  }
}

# gbmVaR, totally 60, equivalent
gbmv2 <- data.frame(port$df.Date)
colnames(gbmv2) <- "Date"
s1 <- "VaR_exp"
for(i in 1:length(horizon)){
  for(j in 1:length(VaRp)){
    for(k in 0:(length(win)-1)){
      mylist <- data.frame(mapply(gbmVaR,s0,port[,indx+length(win)*3+1+3*k],port[,indx+length(win)*3+1+3*k],VaRp[j],horizon[i]))
      s1 <- paste(s1,paste(win_yr[k+1],paste(as.character(VaRp[j]),hor[i],sep="_"),sep="_"),sep = "_")
      colnames(mylist) <- s1
      
      gbmv2 = cbind(gbmv2,mylist)
      s1 <- "VaR_exp"
    }
  }
}

# gbmES, totally 60, equivalent 
gbme2 <- data.frame(port$df.Date)
colnames(gbme2) <- "Date"
s1 <- "ES_exp"
for(i in 1:length(horizon)){
  for(j in 1:length(ESp)){
    for(k in 0:(length(win)-1)){
      mylist <- data.frame(mapply(gbmES,s0,port[,indx+length(win)*3+1+3*k],port[,indx+length(win)*3+1+3*k],ESp[j],horizon[i]))
      s1 <- paste(s1,paste(win_yr[k+1],paste(as.character(ESp[j]),hor[i],sep="_"),sep="_"),sep = "_")
      colnames(mylist) <- s1
      
      gbme2 = cbind(gbme2,mylist)
      s1 <- "ES_exp"
    }
  }
}   
