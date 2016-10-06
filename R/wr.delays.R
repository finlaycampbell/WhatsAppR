wr.delays <- function(data){
  
  delays <- data.frame(Person=factor(rep(NA,nrow(data)),levels=levels(data$person)),time=rep(NA,nrow(data)))
  
  counter <- 1
  
  for(i in 2:nrow(data)){
    if(data$person[i] != data$person[i-1]){
      delays$Person[counter] <- data$person[i]
      delays$time[counter] <- as.numeric(difftime(data$fulltime[i],data$fulltime[i-1],units="mins"))
      counter <- counter + 1
    }
  }
  
  delays <- delays[1:(min(which(is.na(delays$Person)))-1),]
  
  if(sum(delays$time<0) > 0) delays <- delays[-which(delays$time<0),]
  
  return(delays)
  
}