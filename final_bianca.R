library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)


all_tweet_text  <- all_tweets %>%
  select(text) %>%
  pull()


Analisis_T <- reads.rds

data_frame(all_tweets)

files.rds <- list.files(path = ".", pattern = "*.rds")

all_tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()



all_tweet_text <- Corpus(VectorSource(all_tweet_text))
#inspect(all_tweet_text)

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

remueve_palabras <- c(
  "años", "hashtag", "miprimeracoso", "topic", "first", "women", "iba", "thousands", "lugar",
  "tuits", "h...", "twitter", "d...", "...", "vivecocafmniallday", "tweeted", "about", "gtgt",
  "gameofthronesseasonvrrumboalcampeonatospanishgpconcachampionsenmdmarco", "against", "h...",
  "fabi...", "n...", "“", "➡", "”", "llavesdedos…", "…", "internetimportancia", "denounce"
)
all_tweet_text <- tm_map(all_tweet_text, toSpace, "https\\S*")
all_tweet_text <- tm_map(all_tweet_text, toSpace, "/")
all_tweet_text <- tm_map(all_tweet_text, toSpace, "\\|")
all_tweet_text <- tm_map(all_tweet_text, toSpace, "RT")
all_tweet_text <- tm_map(all_tweet_text, toSpace, "…")
all_tweet_text <- tm_map(all_tweet_text, toSpace, "@.*:")
all_tweet_text <- tm_map(all_tweet_text, toSpace, "#MiPrimerAcoso")

all_tweet_text <- tm_map(all_tweet_text, content_transformer(tolower))
all_tweet_text <- tm_map(all_tweet_text, removeNumbers)
all_tweet_text<- tm_map(all_tweet_text, removeWords, stopwords("spanish"))
all_tweet_text <- tm_map(all_tweet_text, removeWords, remueve_palabras)
all_tweet_text <- tm_map(all_tweet_text, removePunctuation)
all_tweet_text <- tm_map(all_tweet_text, stripWhitespace)

dtm <- TermDocumentMatrix(all_tweet_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(
  words = d$word,
  freq = d$freq,
  min.freq = 1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)