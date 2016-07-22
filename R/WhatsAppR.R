

library(ggplot2)
library(reshape2)
library(chron)
library(wordcloud)
library(tm)
library(R.utils)

WhatsAppR <- function(file,save=FALSE,OS="android",wordlcoud=TRUE){
  
  input <- readLines(file)

  data <- wr.data(input,OS)
    
  delays <- wr.delays(data)

  plots <- wr.plots(data,save)
  
  if(wordlcoud) wr.wordcloud(data)

  out <- list()
  out$data <- list(data=data,delays=delays)
  out$plots <- plots

  return(out)
}

for(i in c("K","M","C","CC","AIDS","TC")) assign(paste("out",i,sep=""),WhatsAppR(paste("chat",i,sep=""),save=TRUE))