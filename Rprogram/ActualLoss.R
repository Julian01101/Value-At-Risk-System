# Return actualLoss and Exceptions
ActualLoss <- function(s0,horizon,prices){
  end = length(prices)
  loss<-vector()
  for(i in 1:horizon){
    loss = c(loss,0)
  }

  for(i in i:(end-horizon)){

    loss = c(loss,s0 - (s0/prices[i+horizon])*prices[i])

  }
  return(loss)
}