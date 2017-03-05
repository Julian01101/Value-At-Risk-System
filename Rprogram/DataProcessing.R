##############################################################################################
# Data Processing
# 1. Read Stock Data and Implied Volatility Data from csv
# 2. Calculate Options Prices by BS model
# 3. Specify positions on each asset by user
# 4. Program generates shares on Stocks and Options
# 5. Portfolio Values
#
# Example Data Description
#
# 4 stocks - apple,ibm,ko,pep
# for each stock, we have 3-month put implied volatility, 6-month put implied volatility,
# 12-month put implied volatility,3-month call implied volatility,6-month put implied volatility,
# 12-month call implied volatility
# The whole portfolio consists of 4 stocks and 24 options, each have two trading direction 
# - long and short
# User could specify which asset they do not want to include in the portfolio by set position of 
# the asset to zero.
#############################################################################################
source("UserDefinedInputs.R")

library(readr)
library(rpsychi)

data_example <- read_csv("C:/Users/Meng Bai/Desktop/projects Fall2016/DataSet/data_example.csv", 
                         col_types = cols(Date = col_date(format = "%m/%d/%Y")))

# Please enter trading period

df <- data_example[1:tradedays,]
if(tradedays > dim(data_example)[1]){
  tradedays = dim(data_example)[1]
}

# Copies of dataframe, options data only to row 3045 in the example
# However, we have 20 years of stock data
# The following dataframes are for calibration gbm to stocks

dfs <- data_example[1:9000,]
dfs2 <- data_example[1:9000,]

##### Caculate Option Prices###### 

# at T = 0, the time to maturities are 3 months,6 months, and 12 months (in years, 3/12 etc.)

put_price <- data.frame(df$Date)
call_price <- data.frame(df$Date)
colnames(put_price) <- "Date"
colnames(call_price) <- "Date"
dput <- data.frame(df$Date)
dcall <- data.frame(df$Date)
colnames(dput) <- "Date"
colnames(dcall) <- "Date"

# at T + horizon, the time to maturity of options becomes 3 months, 
# 6 months and 12 months minus horizon(such as, 3/12 - 5/252) 

put_price_t <- data.frame(df$Date)
call_price_t <- data.frame(df$Date)
colnames(put_price_t) <- "Date"
colnames(call_price_t) <- "Date"
dput_t <- data.frame(df$Date)
dcall_t <- data.frame(df$Date)
colnames(dput_t) <- "Date"
colnames(dcall_t) <- "Date"

# Generate Labels for each put option
optlab1<- vector()
optlab2<- vector()
expiration <- c("3MO","6MO","12MO")
stocklab <- colnames(df)[2:(nstock+1)]

# Generate list of option names
for(i in 1:length(stocklab)){
  for(j in 1:length(expiration)){
    optlab1 <- c(optlab1,paste(paste(stocklab[i],expiration[j],sep="_"),"Put",sep = "_"))
    optlab2 <- c(optlab2,paste(paste(stocklab[i],expiration[j],sep="_"),"Call",sep = "_"))
  }
}

source("bsPut.R")
source("deltaPut.R")
source("bsCall.R")
source("deltaCall.R")

# Specify risk free rate
# at T = 0
rf = 0.005
for(j in 1:nstock){
  for(i in 0:2){
    
    px_last <- as.numeric(data.matrix(df[,j+1]))
    imvol <- as.numeric(data.matrix(df[,(1+nstock+1)*j+i]))
    imvol2 <- as.numeric(data.matrix(df[,(1+nstock+1)*j+i+nexpir]))
    put_price <- cbind(put_price,mapply(bsPut,px_last,px_last,expir[i+1],imvol/100,rf))
    call_price <- cbind(call_price,mapply(bsCall,px_last,px_last,expir[i+1],imvol2/100,rf))
    dput <- cbind(dput,mapply(deltaPut,px_last,px_last,expir[i+1],imvol/100,rf))
    dcall <- cbind(dcall,mapply(deltaCall,px_last,px_last,expir[i+1],imvol2/100,rf))
  }
}
colnames(put_price) <- c("Date",optlab1)
colnames(call_price) <- c("Date",optlab2)
colnames(dput) <- c("Date",optlab1)
colnames(dcall) <- c("Date",optlab2)

# at T + horizon

rf = 0.005
for(j in 1:nstock){
  for(i in 0:2){
    
    px_last <- as.numeric(data.matrix(df[,j+1]))
    imvol <- as.numeric(data.matrix(df[,(1+nstock+1)*j+i]))
    imvol2 <- as.numeric(data.matrix(df[,(1+nstock+1)*j+i+nexpir]))
    put_price_t <- cbind(put_price_t,mapply(bsPut,px_last,px_last,expir[i+1]-horizon,imvol/100,rf))
    call_price_t <- cbind(call_price_t,mapply(bsCall,px_last,px_last,expir[i+1]-horizon,imvol2/100,rf))
    dput_t <- cbind(dput_t,mapply(deltaPut,px_last,px_last,expir[i+1]-horizon,imvol/100,rf))
    dcall_t <- cbind(dcall_t,mapply(deltaCall,px_last,px_last,expir[i+1]-horizon,imvol2/100,rf))
  }
}
colnames(put_price_t) <- c("Date",optlab1)
colnames(call_price_t) <- c("Date",optlab2)
colnames(dput_t) <- c("Date",optlab1)
colnames(dcall_t) <- c("Date",optlab2)


# stock and Option price on 2004-10-25
callpx <- as.vector(call_price[tradedays,2:dim(call_price)[2]])
putpx <- as.vector(put_price[tradedays,2:dim(put_price)[2]])
stockpx <- as.vector(data_example[tradedays,2:(nstock+1)])

#### Shares of each asset
#### No rebalancing 

shares_sl = round(sl/stockpx)
shares_ss = round(ss/stockpx)
shares_cl = round(cl/callpx)
shares_cs = round(cs/callpx)
shares_pl = round(pl/putpx)
shares_ps = round(ps/putpx)

### Data Set Output
### 
asset_prices = cbind(df[,1:(nstock+1)],put_price[,2:dim(put_price)[2]],put_price_t[,2:dim(put_price)[2]],
                     call_price[,2:dim(call_price)[2]],call_price_t[,2:dim(call_price)[2]])
write.csv(asset_prices, file = "assetprices.csv")
#write.csv(cbind(dput,dcall), file = "delta.csv")