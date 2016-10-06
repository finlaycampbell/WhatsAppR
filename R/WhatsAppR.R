##' Visualising WhatsApp conversation data
##' 
##' @author Finlay Campbell <f.campbell15@@imperial.ac.uk>
##' 
##' @param file the location of the WhatsApp txt file (including \\ replacement)
##' @param save a logcal indicating if the output plots should be saved to the current directory
##' @param OS indicating if the operating system is "android" or "iOS"
##' @param wordcloud a logical indicating if a wordcloud should be plotted or not 

library(ggplot2)

WhatsAppR <- function(file,save=FALSE,OS="android",wordlcoud=TRUE){
  
  input <- readLines(file)

  data <- wr.data(input,OS)
    
  delays <- wr.delays(data)

  plots <- wr.plot(data,delays,save)
  
  if(wordlcoud) wr.wordcloud(data)

  out <- list()
  out$data <- list(data=data,delays=delays)
  out$plots <- plots

  return(out)
}

#for(i in c("K","M","C","CC","AIDS","TC")) assign(paste("out",i,sep=""),WhatsAppR(paste("chat",i,sep=""),save=TRUE))