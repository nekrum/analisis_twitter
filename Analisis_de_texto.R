##Cloud text analisis

install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tidytext")

##Librerias

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)

##Creacion y filtrado del texto

text_tweets <- select(carto_data,text)

filter_tweets <-gsub("https\\S*", "", text_tweets), 
gsub("@\\S*", "", text_tweets),
gsub("amp", "", text_tweets),  
gsub("[\r\n]", "", text_tweets), 
gsub("[[:punct:]]", "", text_tweets), 
gsub('\\b+RT', '', text_tweets),
gsub("@\\w+", "", text_tweets)

View(filter_tweets)
View(text_tweets)


tweets_palabras <-  filter_tweets %>%  ##no carga el paquete :/
  unnest_tokens(word, text)

words <- tweets_palabras %>% count(word, sort=TRUE)