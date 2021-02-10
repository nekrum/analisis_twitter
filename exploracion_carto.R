library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


carto_data <- read_csv(
  "table_5d787653.csv",
  col_types = cols(
    the_geom = col_skip(),
    cartodb_id = col_skip(), id = col_skip(),
    media = col_skip(), urls = col_skip(),
    geo = col_skip(), time_zone = col_skip(),
    location = col_skip(), source = col_skip(),
    profile_image = col_skip(), language = col_skip(),
    cartodb_georef_status = col_skip()
  )
)

carto_data %>%
  mutate(dia_pos = day(universal_time_stamp)) %>%
  group_by(dia_pos) %>%
  summarise(numero_tw = n()) %>%
  ggplot(aes(x=dia_pos, y=numero_tw)) +
  geom_col() +
  labs(title="Número de pos para #Miprimeracoso")


library(tm)
library(wordcloud)

corpus_carto <- carto_data %>%
  select(text) %>%
  pull()


corpus_carto <- Corpus(VectorSource(corpus_carto))
#inspect(corpus_carto)

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

corpus_carto <- tm_map(corpus_carto, toSpace, "https\\S*")
corpus_carto <- tm_map(corpus_carto, toSpace, "/")
corpus_carto <- tm_map(corpus_carto, toSpace, "\\|")
corpus_carto <- tm_map(corpus_carto, toSpace, "RT")
corpus_carto <- tm_map(corpus_carto, toSpace, "@.*:")
corpus_carto <- tm_map(corpus_carto, toSpace, "#MiPrimerAcoso")

corpus_carto <- tm_map(corpus_carto, content_transformer(tolower))
corpus_carto <- tm_map(corpus_carto, removeNumbers)
corpus_carto <- tm_map(corpus_carto, removeWords, stopwords("spanish"))
corpus_carto <- tm_map(corpus_carto, removeWords, c("años", "hashtag", "miprimeracoso"))
corpus_carto <- tm_map(corpus_carto, removePunctuation)
corpus_carto <- tm_map(corpus_carto, stripWhitespace)

dtm <- TermDocumentMatrix(corpus_carto)
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

