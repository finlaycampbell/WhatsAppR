
##' Visualising WhatsApp conversation data
##'
##' @author Finlay Campbell <f.campbell15@@imperial.ac.uk>
##'
##' @return Returns a named list of output plots
##' @export
##'
##' @param file the directory of the WhatsApp .txt file
##' @param save a logcal indicating if the output plots should be saved to the current directory
##' @param OS indicating if the operating system is "android" or "iOS"
##' @param plot indicating if plots should be created and return in output
##' @param wordcloud a logical indicating if a wordcloud should be plotted or not
##' @param CheckForErrors will check for errors in reading of input data, mostly due to quoted text

WhatsAppR <- function(file,save=FALSE,OS="android",plot=TRUE,wordcloud=TRUE,CheckForErrors=FALSE){

    data <- wr.data(file,OS,CheckForErrors)

    delays <- wr.delays(data)

    if(plot) plots <- wr.plot(data,delays,save)
    else plots <- NULL

    if(wordcloud) wr.wordcloud(data)

    out <- list()
    out$data <- list(data=data,delays=delays)
    out$plots <- plots

    return(out)
}
