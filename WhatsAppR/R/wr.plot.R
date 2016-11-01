##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 geom_histogram
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 ggsave 
##' @importFrom ggplot2 theme_set
##' @importFrom ggplot2 ggsave
##' 
##' 

wr.plot <- function(data,delays,save){
  
  theme_set(theme_gray(base_size = 18))
  
  delayhist <- ggplot(delays,aes(x=time)) + geom_histogram(aes(fill=Person),alpha=0.5, position="identity",binwidth=1) + 
               xlim(-1,20) + xlab("Time to reply (min)") + ylab("Count") + theme(legend.title = element_blank())
  
  timeline <- ggplot(data,aes(x=date)) + geom_histogram(binwidth=1) + xlab("Date") + ylab("Messages")
  
  daytime <- ggplot(data,aes(x=time)) + geom_histogram(binwidth=0.005) + chron::scale_x_chron(format="%H:%M",n=10) +
             xlab("Time") + ylab("Messages")
  
  plot.msg <- data.frame(Person=levels(data$person),
                         Messages=as.vector(table(data$person)),
                         Words=as.vector(by(data$words,data$person,sum)),
                         Delay=as.vector(by(delays$time,delays$Person,mean)))
  
  plot.msg[,2:ncol(plot.msg)] <- apply(plot.msg[,2:ncol(plot.msg)],2, function(i) i/max(i))
  
  ggplot.msg <- reshape2::melt(plot.msg,id="Person")
  ggplot.msg$Person <- factor(ggplot.msg$Person,levels=levels(data$person))
  ggplot.msg$variable <- factor(ggplot.msg$variable,levels=c("Words","Messages","Delay"))
  
  comparison <- ggplot(ggplot.msg,aes(variable)) + geom_bar(aes(y=value,fill=Person),stat="identity",position="dodge") +
                ylab("Relative value") + theme(axis.title.x=element_blank(),legend.title = element_blank())
  
  if(save){
    ggsave(paste("figs\\",file,"_delayhist.png",sep=""),delayhist)
    ggsave(paste("figs\\",file,"_timeline.png",sep=""),timeline)
    ggsave(paste("figs\\",file,"_daytime.png",sep=""),daytime)
    ggsave(paste("figs\\",file,"_comparison.png",sep=""),comparison)
  }
  
  return(list(delayhist=delayhist,timeline=timeline,daytime=daytime,comparison=comparison))
  
}