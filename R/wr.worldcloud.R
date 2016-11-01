
## Creates a wordlcoud from 'data', excluding common words

wr.wordcloud <- function(data){
  
  rm.words <- c(tm::stopwords(),R.utils::capitalize(tm::stopwords()),"<Media","omitted>")
  
  words.table <- sort(table(unlist(strsplit(data$text," "))),decreasing=TRUE)[1:500]
  words.table <- words.table[!names(words.table) %in% rm.words]
  
  cloud <- wordcloud::wordcloud(names(words.table),words.table,c(4,0.5))
  
  return(cloud)
}