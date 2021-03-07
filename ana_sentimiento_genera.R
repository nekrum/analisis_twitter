
#No olvidar cargar paqueterias y diccionario

library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)
library(wordcloud2)


library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

library(tm)
#library(NPL)
library(SnowballC)
#library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(knitr)
library (tidyr)
library(plotly)
library(hrbrthemes)
#Se crea un Dataframe con las pabras de diccionario que se encontraron en el texto de tweet
all_tweets

 
diccionario<- `13428_2015_700_MOESM1_ESM.(1)`
 
diccionario_fil <- diccionario %>% 
  filter(!between(ValenceMean,4,6.07))

tuits_diccionario <- 
  all_tweets %>%
  unnest_tokens(input = "text", output = "Word") %>%
  inner_join(diccionario_fil, ., by = "Word") %>%
  mutate(Tipo = ifelse( ValenceMean > 5 , "Positiva", "Negativa")) 


#graficos

g <- tuits_diccionario %>%
  count(Word, Tipo, sort = TRUE) %>%
  ungroup()


g %>%
  group_by(Tipo) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(n, Word, fill = Tipo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#Nube de palabras

Negativas <- g %>% 
  filter(Tipo == "Negativa") %>% 
  select(-2)


g_2 <- tuits_diccionario %>%
  count(Word, Tipo, sort = TRUE) %>%
  ungroup()

Positivas <- g %>% 
  filter(Tipo == "Positiva") %>% 
  select(-2)

wordcloud2(data=Negativas, size = 2.3, minRotation = -pi/230, maxRotation = -pi/230, 
           rotateRatio = 1, shape = "circle")

wordcloud2(data=Positivas, size = 2.3, minRotation = -pi/230, maxRotation = -pi/230, 
           rotateRatio = 1, shape = "circle")

letterCloud( g, word = "8M", color='random-light' , backgroundColor="purpure")

letterCloud( Negativas, word = "PEACE", color="white", backgroundColor="pink")


#En caso de que no corra con el WordCloud2 hacer la siguiente instalacion para 
# desistalar y volverlo a cargar 
#library(devtools)
#devtools::install_github("lchiffon/wordcloud2")
#letterCloud(d


all_tweet_text  <- all_tweets %>%
  select(text) %>%
  pull()


Analisis_T <- reads.rds

data_frame(all_tweets)

files.rds <- list.files(path = ".", pattern = "*.rds")

all_tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()



all_tweet_text <- Corpus(VectorSource(all_tweet_text))
#inspect(all_tweet_text)

toSpace_2 <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

remueve_palabras_2 <- c("años", "hashtag", "miprimeracoso", "topic", "first", "women", "iba", "thousands", "lugar",
                        "tuits", "h...", "twitter", "d...",  "vivecocafmniallday", "tweeted", "about", "gtgt",
                        "gameofthronesseasonvrrumboalcampeonatospanishgpconcachampionsenmdmarco", 
                        "gameofthronesseasonrumboalcampeonatospanishgpconcachampionsenmdmarco",
                        "against", "h...","vivecocafmniallday",
                        "fabi...", "n...", "llavesdedos.", "internetimportancia", "denounce", "miles", 
                        "mientras", "luego", "leer", "historias", "cuenta", "vuelvan",
                        "decir", "chequen", "ches", "historia", "creadora", "después", "cómo", "cada", "hacer", 
                        "leyeron","narraron", "país", "sólo", "dijeron", "casi", "gran", "trending", 
                        "parte", "hacia","tenia", "haber", "acos",
                        "estalloredes", "time", "aunque", "caricaturaoyemathias", 
                        "tendencia", "unas", "catalinapordios", "tampoco", "leyendo", "solo", "cuentan", 
                        "fabi", "forma", "aqu?", "campaña","decía", "denunciamejor", "númeroprimera","primeravioletagobiernocdmx",
                        "siganvenga", "primera","primer","hechi","debate","ahora", "respecto", "nadie",
                        "encedi?", "muchas","bien","corresistas", "narrada", "nunca", "elfraude","bajo","importante", "casa",
                        "lafarsa", "entrampada","usando", "abusos", "menos", "culpa","edad",
                        "forman","siento","ámbito","cuándo","escapabapresunto","promedio","frente",
                        "peor", "ma?ana", "fuerte","pueden","rapasucia", "cuentanos","mira", "sufrido","trascendiómujer",
                        "media","ecuador","usar","episodiowarmis", "cosas","acosaron","metío","imagínate",
                        "sino", "proceso","tener","deberían", "toda", "pase","hombre","números","corresistas",
                        "movimientosevidenciadom?xico","enportada","simematanmetoomx","ropasucia",
                        "edades","cambiar","contamospodría","recuerdas","completo",
                        "ejercicio","puede","comienzan", "calle","casos","escale", "alguna","pensar",
                        "yotambien","pasó","personalhospital","favor","acosadas","mayoría", "dhdesarrollolocal",
                        "elfestortura","ellasdeciden","seguir","tema","sexuales","minutos","todavía",
                        "vivió", "arrojó","edad","mismo", "salgan","inaceptablesindignasabusosigamos","paso", "sucedió", 
                        "criaronideatrapos","lavan","bolivianas", "empiezo","acosada","darse",
                        "hablan", "hacemos","rabia","mucha","primero","sociales","agarró","alguien",
                        "exposicion", "veces","adultos","gigante", "impresionanteiniciativapoder",
                        "llevar", "provoca","quiero","tiempo","used","contexto","felicitaciones"
)

all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "https\\S*")
all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "/")
all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "\\|")
all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "RT")
all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "…")
all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "@.*:")
all_tweet_text <- tm_map(all_tweet_text, toSpace_2, "#MiPrimerAcoso")

all_tweet_text <- tm_map(all_tweet_text, content_transformer(tolower))
all_tweet_text <- tm_map(all_tweet_text, removeNumbers)
all_tweet_text<- tm_map(all_tweet_text, removeWords, stopwords("spanish"))
all_tweet_text <- tm_map(all_tweet_text, removeWords, remueve_palabras)
all_tweet_text <- tm_map(all_tweet_text, removePunctuation)
all_tweet_text <- tm_map(all_tweet_text, stripWhitespace)

dtm_2 <- TermDocumentMatrix(all_tweet_text)
m_2 <- as.matrix(dtm_2)
v_2 <- sort(rowSums(m_2),decreasing=TRUE)
d_2016_2021 <- data.frame(word = names(v_2),freq=v_2)
head(d_2016_2021, 10)

###Wordcloud 2

wordcloud2(data=d_2016_2021, size=1.6)

letterCloud(d_2016_2021, word = "acoso", color='random-light' , backgroundColor="black")

wordcloud2(d_2016_2021, figPath = "/Users/Nurin/Downloads/klipartz.com.png", size = 1, color = "skyblue", backgroundColor="black")

wordcloud2(d_2016_2021, figPath = "/Users/Nurin/Downloads/Feminist Mouse Pads _ Zazzle.png", size = 1, color = "skyblue", backgroundColor="green")

wordcloud2(d_2016_2021, figPath = "/Users/Nurin/Downloads/acoso_3.png", size = .5, color = "black", backgroundColor="white")



