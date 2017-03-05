Exceptions <- function(loss,var){
  end = length(var)
  except <- vector()
  for(i in 1:(end - 252)){
    except = c(except,length(which(loss[i:(i+252)] > var[i:(i+252)])))
  }
  return(except)
}