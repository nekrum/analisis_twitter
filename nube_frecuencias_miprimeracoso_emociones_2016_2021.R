#library(readr)
#library(dplyr)
#library(ggplot2)
#library(lubridate)

#library(tm)
#library(SnowballC)
#library(wordcloud)
#library(RColorBrewer)
#library(dplyr)
#library(tidytext)

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
                        "tuits", "h...", "twitter", "d...", "...", "vivecocafmniallday", "tweeted", "about", "gtgt",
                        "gameofthronesseasonvrrumboalcampeonatospanishgpconcachampionsenmdmarco", "gameofthronesseasonrumboalcampeonatospanishgpconcachampionsenmdmarco",
                        "against", "h...","vivecocafmniallday",
                        "fabi...", "n...", "“", "➡", "”", "llavesdedos…", "…", "internetimportancia", "denounce", "miles", "mientras", "luego", "leer", "historias", "cuenta", "vuelvan",
                        "decir", "chequen", "ches", "historia", "creadora", "después", "cómo", "cada", "hacer", "leyeron","narraron", "país", "sólo", "dijeron", "casi", "gran", "trending", "parte", "hacia","tenia", "haber", "acos",
                        "estalloredes", "time", "aunque", "caricaturaoyemathias", "tendencia", "unas", "catalinapordios", "tampoco", "leyendo", "solo", "cuentan", "fabi", "forma", "aquí"
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
all_tweet_text <- tm_map(all_tweet_text, removeWords, remueve_palabras_2)
all_tweet_text <- tm_map(all_tweet_text, removePunctuation)
all_tweet_text <- tm_map(all_tweet_text, stripWhitespace)

dtm_2 <- TermDocumentMatrix(all_tweet_text)
m_2 <- as.matrix(dtm_2)
v_2 <- sort(rowSums(m_2),decreasing=TRUE)
d_2016_2021 <- data.frame(word = names(v_2),freq=v_2)
head(d_2016_2021, 10)

set.seed(1234)
wordcloud(
  words = d_2016_2021$word,
  freq = d_2016_2021$freq,
  min.freq = 100,
  max.words=200,
  random.order=FALSE,
  rot.per=0.50,
  colors=brewer.pal(8, "PuOr")
)

## Analisis frecuencias palabras mayores a 300 repeticiones intento Daniel

d_2016_2021 %>%
  filter(d_2016_2021$freq > 200) %>%
  ggplot(aes(x= reorder(word, freq), y=freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Tweets #miprimeracoso 2016-2021") + 
  labs(y = "Num. tweets") + 
  labs(x= NULL) + 
  theme_bw() 

##### Analisis de emociones negativas

emo_negativas_2016_2021 <- filter(d_2016_2021, word %in% c("tensión","duda", "ira", "dolor", "aburrimiento", "frustración", "desagrado", "aversión", "tristeza", "triste", "odio","miedo",
                                                 "apatía", "agotamiento", "humillación", "desasosiego","inquietud",
                                                 "preocupación","ansiedad","impaciencia","intranquilidad","desazón","agitación", "ansia", "alarma", 
                                                 "perturbación", "opresión", "tormento", "agobio", "desconfianza","turbación", "inseguridad", "perplejidad", 
                                                 "indecisión", "vacilación", "incertidumbre", "desorientación","fuscación", "confusión", "desconcierto", "perplejidad", 
                                                 "atolondramiento", "aturdimiento", "zozobra","incredulidad", "irritación", "enfado", "enojo", "indignación", "coraje",
                                                 "saña", "crueldad", "rabia", "furia", "furor", "venganza", "cólera","amargura", "aspereza", "aterido", "aflicción", 
                                                 "molestia", "sufrimiento", "mortificación", "tribulación", "congoja", "tormento", "indiferencia", "tedio", "enfado",
                                                 "disgusto","fastidio", "hastío", "hartazgo", "desesperación", "exasperación", "desaliento", "decepción","desengaño","revés","fracaso",
                                                 "malestar", "insatisfacción", "irritación", "hastío","fastidio", "inapetencia","saciedad", "empalago", "saturación","asco", "vasca",
                                                 "animadversión", "aborrecimiento","desgana", "repugnancia", "repulsión", "repugnancia", "rechazo","desdén", "aflicción", "pesar", "nostalgia", 
                                                 "culpa", "depresión", "melancolía", "amargura", "duelo", "congoja", "soledad", "desdicha", "abatimiento", "desconsuelo","agonía",
                                                 "antipatía", "rivalidad", "oposición", "resentimiento", "despecho", "desdén", "desprecio", "burla", "rencor", "celos", "envidia",
                                                 "aprensión", "sospecha", "recelo", "temor", "consternación", "espanto", "terror", "coraje", "pánico","pavor", "desaliento",
                                                 "desgano", "desanimo", "desidia", "flojera", "dejadez", "negligencia", "indiferencia", "frialdad", "abulia", "pereza", "indolencia", 
                                                 "debilidad", "somnolencia", "languidez", "cansancio", "fatiga", "desmayo", "extenuación", "colapso", "sopor", "resignación", "apocamiento", 
                                                 "sometimiento","sumisión", "postración", "deshonra", "verguenza", "vergûenza", "timidez", "sonrojo", "modestia", "sola", "doloroso", "lloré",
                                                 "terrible", "rehuir", "indignante", "pena"))

set.seed(1234)
wordcloud(
  words = emo_negativas_2016_2021$word,
  freq = emo_negativas_2016_2021$freq,
  min.freq = 1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.20,
  colors=brewer.pal(8, "PuOr")
)

emo_negativas_2016_2021 %>%
  filter(emo_negativas_2016_2021$freq > 1) %>%
  ggplot(aes(x= reorder(word, freq), y=freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Emociones #miprimeracoso 2016-2021") + 
  labs(y = "Num. menciones") + 
  labs(x= NULL) + 
  theme_bw() 

#Analisis emociones positivas 

emo_positivas_2016_2021 <- filter(d_2016_2021, word %in% c("calma","quietud", "sosiego","despreocupación", "tranquilidad", "paciencia", "reposo", "placidez","relajación", "alivio", "armonía", "serenidad","impasibilidad", 
                                                 "consuelo", "paz", "certeza", "confianza", "claridad", "seguridad","decisión","creencia", "disposición", "certidumbre","convencimiento", "convicción", "esperanza",
                                                 "firmeza", "resolución","lucidez","fe", "compasión", "conmiseración", "lástima", "mansedumbre","misericordia", "piedad","clemencia","condolencia","abnegación", "caridad",
                                                 "altruismo", "generosidad", "placer", "dulzura", "suavidad", "calidez", "gusto", "gozo","solaz", "esparcimiento", "deleite", "fruición", "diversión", "sorpresa", "distracción",
                                                 "entretenimiento", "esparcimiento", "recreo", "solaz","impresión", "asombro", "pasmo", "satisfacción", "saciedad", "exito", "triunfo", "plenitud", "euforia", "orgullo",
                                                 "agrado", "bienestar", "beneplcito", "satisfacción", "complacencia", "contento", "contenta", "halago", "deseo", "apetencia", "ansia", "antojo", "ilusión", "esperanza", "aspiración",
                                                 "apetito", "interés", "gana", "anhelo","capricho", "avidez","afán","codicia", "avaricia", "ambición", "alegría", "alborozo", "jovialidad", "gozo","fruición", "regocijo","júbilo", 
                                                 "entusiasmo", "exaltación", "felicidad", "dicha", "euforia", "arrebato", "arrobamiento", "éxtasis", "amor", "simpatía","interés", "aprecio","amistad","afición","ternura", "afecto", 
                                                 "estimación", "cariño", "apego", "adoración", "idolatría", "valor", "esfuerzo", "impetu", "brío", "denuedo", "excitación","audacia","osadía","furor", "entusiasmo", "aliento","inspiración",
                                                 "propósito","animación","voluntad","diligencia","ánimo","espíritu","vehemencia","vigor","viveza","fortaleza","energía", "ardor","fogosidad","impetuosidad","altivez","elevación","exaltación",
                                                 "soberbia","dignidad", "honra","arrogancia","orgullo","engreimiento","atrevimiento","desvergûenza","osadía"))

set.seed(1234)
wordcloud(
  words = emo_positivas_2016_2021$word,
  freq = emo_positivas_2016_2021$freq,
  min.freq = 1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.10,
  colors=brewer.pal(8, "PuOr")
)

emo_positivas_2016_2021 %>%
  filter(emo_positivas_2016_2021$freq > 1) %>%
  ggplot(aes(x= reorder(word, freq), y=freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Emociones #miprimeracoso 2016-2021") + 
  labs(y = "Num. menciones") + 
  labs(x= NULL) + 
  theme_bw() 


##checar bag of words / lapplace coefficient / word to get /rex ex
