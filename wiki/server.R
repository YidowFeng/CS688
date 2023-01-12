# Example: Shiny app that search Wikipedia web pages
# server.R
library(shiny)
library(tm)
library(stringi)
library(proxy)
source("WikiSearch.R")
library(wordcloud)

shinyServer(function(input, output) {
  #wordcloud_rep <- repeatable(wordcloud)
  output$distPlot <- renderPlot({
    # Progress Bar while executing function
    result <- SearchWiki(input$select)
    wordcloud(names(result), result, max.words = 50, 
              colors=brewer.pal(8, "Dark2"))
    
  })
})