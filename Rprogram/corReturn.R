library(zoo)

corReturn <- function(price1,price2,n,window){
  cor1 <- vector()
  rtn1   = -diff(log(price1))[1:n]
  rtn2   = -diff(log(price2))[1:n]
  for(i in 1:(n-window)){
    
    #print(rtn1)
    cor1 <- c(cor1, cor(rtn1[i:(i+window -1)],rtn2[i:(i+window -1)]))
  }
  return(cor1)
}
