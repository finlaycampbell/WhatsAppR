
## Cleaning the input data and allocating it to person, text, words and time

wr.data <- function(input,OS){
  
  data <- data.frame(date=as.Date(substr(input,1,10),"%d/%m/%Y"))
  
  if(OS=="android"){
    find.index <- 2
    start_name <- 21
    data$time <- chron::times(paste(substr(input,13,17),":00",sep=""))
  }
  
  if(OS=="iOS"){
    find.index <- 4
    start_name <- 24
    data$time <- chron::times(substr(input,13,20))
  }
  
  data$person <- sapply(input,function(line) substr(line,start_name,gregexpr(":", line)[[1]][find.index]-1))
  
  data$text <- sapply(input,function(line) substr(line,gregexpr(":", line)[[1]][find.index]+2,nchar(line)))
  
  data$words <- sapply(data$text,function(line) length(strsplit(line," ")[[1]]))
  
  data$fulltime <- strptime(paste(data$date,data$time),format='%Y-%m-%d %H:%M')
  
  data <- data[order(data$fulltime),]
  
  #Remove 'people' that are found less than ten times (error when you have pasted wa conversations in chat)
  people <- names(sort(table(data$person)[table(data$person)>10],decreasing=TRUE))
  
  data$person <- factor(data$person,levels=people)
  
  errors <- !data$person %in% people
  
  if(sum(errors) > 0.1*nrow(data)) stop("More than 10% of messages can't be read, error in input format")
  
  data <- data[!errors,]
  
  return(data)
  
}