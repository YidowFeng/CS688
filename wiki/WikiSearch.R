# Example: Shiny app that search Wikipedia web pages
# Wikipedia Search
library(tm)
library(stringi)
library(WikipediR)
SearchWiki <- function (titles) {
  articles <- lapply(titles,function(i) page_content("en","wikipedia", page_name =
                                                       i,as_wikitext=TRUE)$parse$wikitext)
  docs <- Corpus(VectorSource(articles)) # Get Web Pages' Corpus
  remove(articles)
  # Text analysis - Preprocessing
  transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
  temp <- tm_map(docs, transform.words, "<.+?>", " ")
  temp <- tm_map(temp, transform.words, "\t", " ")
  temp <- tm_map(temp, content_transformer(tolower)) # Conversion to Lowercase
  temp <- tm_map(temp, stripWhitespace)
  temp <- tm_map(temp, removeWords, stopwords("english"))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
  remove(docs)
  # Create Dtm
  dtm <- DocumentTermMatrix(temp)
  dtm <- removeSparseTerms(dtm, 0.4)
  dtm$dimnames$Docs <- titles
  
  docsdissim <- dist(as.matrix(dtm), method = "euclidean") # Distance Measure
  #h <- hclust(as.dist(docsdissim), method = "ward.D2") # Group Results

  m <- as.matrix(dtm)
  f<- rowSums(m)
  order <- order(f, decreasing = TRUE)
  head <- head(order, n = 50)
  re <- f[head]
  

}