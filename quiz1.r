library(xlsx)
library(tm)
en_US_twitter <-  "~en_US.twitter.txt"
f <- file(en_US_twitter, open="rb")
nlines <- 0L
while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
  nlines <- nlines + sum(chunk == as.raw(10L))
  print(chunk)
}
print(nlines)
close(f)
#get number of lines
formatC(nlines, format="d", big.mark=",")

max_length <- 0
# reading line by line
processFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1, skipNul= TRUE)
    
    if ( length(line) == 0 ) {
      break
    }
    #print(nchar(line))
    if(nchar(line)>max_length){
      print(nchar(line))
      max_length <- nchar(line)
    }
  }
  
  close(con)
  return(max_length)
}

processFile(en_US_twitter)

en_US_blogs <-  "~en_US.blogs.txt"

en_US_news <-  "~en_US.news.txt"

max_blog <- processFile(en_US_blogs)
max_news <- processFile(en_US_news)


#count number of rows containing 'love' and 'hate' respectively
processWordRow = function(filepath, inputWord) {
  rows_of_word <- 0
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1, skipNul= TRUE)
    
    if ( length(line) == 0 ) {
      break
    }
    #print(nchar(line))
    if(grepl(inputWord,line)){
      rows_of_word <- rows_of_word + 1
    }
  }
  close(con)
  print(rows_of_word)
  return(rows_of_word)
}

love_row <- processWordRow(en_US_twitter, "love") #90956
hate_row <- processWordRow(en_US_twitter, "hate") #
love_row / hate_row


processWordActual = function(filepath, inputWord) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1, skipNul= TRUE)
    
    if ( length(line) == 0 ) {
      break
    }
    #print(nchar(line))
    if(grepl(inputWord,line)){
      print(line)
    }
  }
  close(con)
  return(line)
}
# get actual text
processWordActual(en_US_twitter, "biostats")

# 
processWordActual(en_US_twitter, "A computer once beat me at chess, but it was no match for me at kickboxing")
