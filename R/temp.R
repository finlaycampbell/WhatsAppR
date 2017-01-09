kitten <- function(width, height, destfile, ...) {
  url <- sprintf("http://placekitten.com/g/%d/%d", width, height)
  download.file(url, destfile, ..., mode="wb")
  destfile
}