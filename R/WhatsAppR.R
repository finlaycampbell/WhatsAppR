
##' Visualising WhatsApp conversation data
##' 
##' @author Finlay Campbell <f.campbell15@@imperial.ac.uk>
##' 
##' @export
##' 
##' @param file the location of the WhatsApp txt file (including \\ replacement)
##' @param save a logcal indicating if the output plots should be saved to the current directory
##' @param OS indicating if the operating system is "android" or "iOS"
##' @param wordcloud a logical indicating if a wordcloud should be plotted or not 

WhatsAppR <- function(file,save=FALSE,OS="android",wordcloud=TRUE){
  
  input <- readLines(file)

  data <- wr.data(input,OS)
    
  delays <- wr.delays(data)

  plots <- wr.plot(data,delays,save)
  
  if(wordcloud) wr.wordcloud(data)

  out <- list()
  out$data <- list(data=data,delays=delays)
  out$plots <- plots

  return(out)
}