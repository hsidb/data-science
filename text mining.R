austen <- Corpus(Workbook1, list(language = "en"))
install.packages("tm")
install.packages("wordcloud")
install.packages("tm.plugin.webmining")
install.packages("topicmodels")
install.packages("SnowballC")

library(tm)
Loading required package: NLP
library(wordcloud)
Loading required package: RColorBrewer
library(tm.plugin.webmining)
Attaching package: ‘tm.plugin.webmining’
The following object is masked from ‘package:base’:
  parse
 library(topicmodels)
 library(SnowballC)

txttrans = function(text){
  + text = tm_map(text, content_transformer(tolower))
  + text = tm_map(text, removePunctuation)
  + text = tm_map(text, content_transformer(removeNumbers))
    + text = tm_map(text, removeWords, stopwords("english"))
    + text = tm_map(text, content_transformer(stripWhitespace))
    + text = tm_map(text, stemDocument) }