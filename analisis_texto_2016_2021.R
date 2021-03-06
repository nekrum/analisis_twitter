#libreria

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

library(tm)
#library(NPL)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(knitr)
library (tidyr)
library(plotly)
library(hrbrthemes)
#Exploration 
----------------------------------------------------------------------
#Primer tweet

p_tweet <- all_tweets %>% 
  arrange(desc(created_at)) %>% slice(n()) %>% 
  pull(created_at)

#ultimo tweet
u_tweet <- all_tweets %>% 
    arrange(created_at) %>% slice( n()) %>% 
     pull(created_at)

ntweets <-- nrow(all_tweets)

n_year <-- as.numeric(p_tweet - u_tweet)

statusPorYear<- nrow(all_tweets)/ n_year 


date_tweets_2 <- date_tweets %>% mutate(observation = 1:n())

----------------------------------------------------------------
# Limpieza

# Seleccion de variables

#all_tweets <- all_tweets %>% select( created_at, status_id, text)

# Se renombran las variables con nombres m?s pr?cticos
#all_tweets <- all_tweets %>% rename( fecha = created_at,
                            texto = text)
#head(all_tweets)

base_exel_all_t <- as.data.frame(lapply(all_tweets, unlist))

write.csv(all_tweets,
          "C:\\Users\\Nurin\\Documents\\Bianca\\all_tweets.csv" , 
          row.names= FALSE)

-----------------------------------------------------------------------
Analisis_T <- reads.rds

data_frame(all_tweets)

files.rds <- list.files(path = ".", pattern = "*.rds")


all_tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()

data_frame_2 <- (all.tweets)

files.rds <- list.files(path = ".", pattern = "*.rds")


all.tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()

-----------------------------------------------------------------------
all_tweets %>% 
  mutate(as_year= created_at) %>% 
  group_by (as_year) %>% 
  summarise (ntweets = n())

ggplot(all_tweets, aes(x= created_at))+
  geom_histogram(position = "indetity", bins = 20, show.legend = FALSE)+
  labs(x = "A?o", y= "Numero de Tweets")
theme_bw()
------------------------------------------------------------------------
#Tabla Fx
a?o <-- table(year(all_tweets$created_at))
kable(sort(a?o, descreasing = TRUE),col.names = c ("A?o","Frecuencia"))

mes <-- table(months.Date(all_tweets$created_at) %>% filter(a?o))
kable(sort(mes, descreasing = TRUE),col.names = c ("Mes","Frecuencia"))

dias <--table(weekdays.Date(all_tweets$created_at))
kable(sort(dias, descreasing = TRUE),col.names = c ("D?a","Frecuencia"))

all_tweets$created_at <- as.Date(all_tweets$created_at)

ggplot(all_tweets, aes(x = created_at)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = "Fecha", y = "N?mero de Tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) 
 


#ggplot(all_tweets, aes(x=created_at, y=ntweets)) +
 # geom_col() +
  #labs(title="Número de pos para #Miprimeracoso")
#Separamos la variable created at por año, mes y día
---------------------------------------------------------------------
 date_tweets <- separate (all_tweets , created_at, 
           into  = c("a?o","mes", "d?a","hora", "minuto", "segundo"), 
           remove=FALSE, extra = "merge")
 

ggplot(date_tweets, aes(x= mes )) +
  #scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  #coord_flip() +
  geom_bar () +
  facet_wrap(~ date_tweets$a?o, nrow = 12, ncol = 12) +
  labs(title = "Fecha", x = "", y = "N?mero de Tweets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
-----------------------------------------------------------------------
#grafico con el tiempo

# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# Load dataset from github
#data_tiempo <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
#date_tweets$año <- as.Date(data$date)

scale_fill_manual
# Usual area chart
p <- date_tweets_2 %>%  
  ggplot( aes(x= created_at, y=observation)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "3 month")+
  scale_y_continuous(limit=c(1,12000))+
  #geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  #scale_fill_manual(limits = c ()) +
  geom_area(fill="#69b3a2", alpha= .8) +
  geom_line(color="#69b3a2") +
  xlab ("Fecha publicaci?n")+
  ylab("Tweets Mi primer Acoso") +
 
  #scale_y_continuous(labels = comma)+
  #ylim(1,12650) +
  theme_ipsum()
 

# Turn it interactive with ggplotly
p <- ggplotly(p)
#p <-plot_ly(p)


# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/ggplotlyAreachart.html"))
------------------------------------------------------------------------------
  
  one_plot <- function(d) {
    plot_ly(d, x = ~a?o) %>%
      add_annotations(
        ~unique(a?o), x = 0.5, y = 1, 
        xref = "paper", yref = "paper", showarrow = FALSE
      )
  }

date_tweets_2 %>%
  split(.$a?o) %>%
  lapply(a?o) %>% 
  subplot(nrows = 2, shareX = TRUE, titleX = FALSE) %>%
  hide_legend()

p1 <- plot_ly(date_tweets_2, x = ~a?o) %>%
  add_histogram()
p1_graf <- function( date_tweets_2 = "2016"){
  h <- hist(date_tweets_2, plot = FALSE)
  plot_ly(x= h$a?o, y=h$observaciones) %>%
    add_bars(name = "Mi primer acoso")
    
}
---------------------------------------------------------------------------
 #DATOS POR A?O 
  
  ano2016 <- 
  all_tweets %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = ymd(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2016)
  
ano2017 <- 
  all_tweets %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = ymd(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2017)

  
ano2018 <- 
  all_tweets %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = ymd(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2018)

ano2019 <- 
  all_tweets %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = ymd(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2019)

ano2020 <- 
  all_tweets %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Periodo", "Mes", "Dia"), sep = "-",
           remove = FALSE) %>%
  mutate(Fecha = ymd(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2020)

---------------------------------------------------------------------------------
#Token

Analisis_T <- reads.rds

data_frame(all_tweets)

files.rds <- list.files(path = ".", pattern = "*.rds")

all_tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()


all_tweet_text <- Corpus(VectorSource(all_tweet_text))
#inspect(all_tweet_text)

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

#remover caracteres "indeseables"



remueve_palabras <- c(
  "a?os", "hashtag", "miprimeracoso", "topic", "first", "women", "iba", "thousands", "lugar","&amp",
  "tuits", "h...", "twitter", "d...", "...", "vivecocafmniallday", "tweeted", "about", "gtgt",
  "gameofthronesseasonvrrumboalcampeonatospanishgpconcachampionsenmdmarco", "against", "h...",
  "fabi...", "n...", "“", "➡", "”", "llavesdedos…", "…", "internetimportancia", "denounce", "miles", "mientras", "luego", "leer", "historias", "cuenta", "vuelvan",
  "decir", "chequen", "ches", "historia", "creadora", "después", "cómo", "cada", "hacer", "leyeron","narraron", "país", "sólo", "dijeron", "casi", "gran", "trending", "parte", "hacia","tenia", "haber", "acos",
  "estalloredes", "time", "aunque", "dec?a", "usando", "ahora", "caricaturaoyemathias", "tendencia", "unas", "catalinapordios", "tampoco", "leyendo", "solo", "cuentan", "fabi", "forma", "aqu??"
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

#ignorar tweets

ig_tweets <- all_tweets %>% 
  filter(is_retweet == FALSE) %>% 
  mutate(text=srt_remove_all (text, remove_reg)) %>% 
  unnest_tokens(word,text,token = "tweets") %>% 
  filter(!word %in% lista_stopwords,
         !word %in% str_remove_all (lista_stopwords, "'")) 
--------------------------------------------------------------------------
dtm <- TermDocumentMatrix(all_tweet_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
#Nube de palabras 
set.seed(1234)
wordcloud (
  words = d$word,
  freq = d$freq,
  
  min.freq =100,
  max.words=500,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr"))
----------------------------------------------------------------------

## Analisis frecuencias palabras mayores a 300 repeticiones intento Daniel

d %>%
  filter(d$freq > 500) %>%
  ggplot(aes(word,freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Tweets #miprimeracoso 2016-2021") + 
  labs(y = "Num. repeticiones") + 
  labs(x= NULL) + 
  theme_bw() 

barplot(d [1:200,]$freq, las = 2, 
        names.arg = d[50:500,]$word,
        col ="lightblue", main ="Palabras más frecuentes",
        ylab = "Frecuencia")
