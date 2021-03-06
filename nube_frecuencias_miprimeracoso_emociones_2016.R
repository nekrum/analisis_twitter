library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(NLP)
library(tm)
library(wordcloud)

  
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



corpus_carto <- carto_data %>%
  select(text) %>%
  pull()


corpus_carto <- Corpus(VectorSource(corpus_carto))
#inspect(corpus_carto)

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

remueve_palabras <- c(
  "años", "hashtag", "miprimeracoso", "topic", "first", "women", "iba", "thousands", "lugar",
  "tuits", "h...", "twitter", "d...", "...", "vivecocafmniallday", "tweeted", "about", "gtgt",
  "gameofthronesseasonvrrumboalcampeonatospanishgpconcachampionsenmdmarco", "gameofthronesseasonrumboalcampeonatospanishgpconcachampionsenmdmarco",
  "against", "h...","vivecocafmniallday",
  "fabi...", "n...", "“", "➡", "”", "llavesdedos…", "…", "internetimportancia", "denounce", "miles", "mientras", "luego", "leer", "historias", "cuenta", "vuelvan",
  "decir", "chequen", "ches", "historia", "creadora", "después", "cómo", "cada", "hacer", "leyeron","narraron", "país", "sólo", "dijeron", "casi", "gran", "trending", "parte", "hacia","tenia", "haber", "acos",
  "estalloredes", "time", "aunque", "caricaturaoyemathias", "tendencia", "unas", "catalinapordios", "tampoco", "leyendo", "solo", "cuentan", "fabi", "forma", "aquí"
)
corpus_carto <- tm_map(corpus_carto, toSpace, "https\\S*")
corpus_carto <- tm_map(corpus_carto, toSpace, "/")
corpus_carto <- tm_map(corpus_carto, toSpace, "\\|")
corpus_carto <- tm_map(corpus_carto, toSpace, "RT")
corpus_carto <- tm_map(corpus_carto, toSpace, "…")
corpus_carto <- tm_map(corpus_carto, toSpace, "@.*:")
corpus_carto <- tm_map(corpus_carto, toSpace, "#MiPrimerAcoso")

corpus_carto <- tm_map(corpus_carto, content_transformer(tolower))
corpus_carto <- tm_map(corpus_carto, removeNumbers)
corpus_carto <- tm_map(corpus_carto, removeWords, stopwords("spanish"))
corpus_carto <- tm_map(corpus_carto, removeWords, remueve_palabras)
corpus_carto <- tm_map(corpus_carto, removePunctuation)
corpus_carto <- tm_map(corpus_carto, stripWhitespace)

dtm <- TermDocumentMatrix(corpus_carto)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d_2016 <- data.frame(word = names(v),freq=v)
head(d_2016, 10)

set.seed(1234)
wordcloud(
  words = d_2016$word,
  freq = d_2016$freq,
  min.freq = 50,
  max.words=200,
  random.order=FALSE,
  rot.per=0.35,
  colors=brewer.pal(8, "PuOr")
)

## Analisis frecuencias palabras mayores a 300 repeticiones intento Daniel


d_2016 %>%
  filter(d_2016$freq > 300) %>%
  ggplot(aes(x= reorder(word, freq), y=freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Tweets #miprimeracoso 2016") + 
  labs(y = "Num. repeticiones") + 
  labs(x= NULL) + 
  theme_bw() 

##### Analisis de emociones negativas

    emo_negativas_2016 <- filter(d_2016, word %in% c("tensión","duda", "ira", "dolor", "aburrimiento", "frustración", "desagrado", "aversión", "tristeza", "triste", "odio","miedo",
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
  words = emo_negativas_2016$word,
  freq = emo_negativas_2016$freq,
  min.freq = 1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.210,
  colors=brewer.pal(8, "PuOr")
)

emo_negativas_2016 %>%
  filter(emo_negativas_2016$freq > 1) %>%
  ggplot(aes(x= reorder(word, freq), y=freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Tweets #miprimeracoso 2016") + 
  labs(y = "Num. repeticiones") + 
  labs(x= NULL) + 
  theme_bw() 

#Analisis emociones positivas 

emo_positivas_2016 <- filter(d_2016, word %in% c("calma","quietud", "sosiego","despreocupación", "tranquilidad", "paciencia", "reposo", "placidez","relajación", "alivio", "armonía", "serenidad","impasibilidad", 
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
  words = emo_positivas_2016$word,
  freq = emo_positivas_2016$freq,
  min.freq = 1,
  max.words=200,
  random.order=FALSE,
  rot.per=0.10,
  colors=brewer.pal(8, "PuOr")
)

emo_positivas_2016 %>%
  filter(emo_positivas_2016$freq > 1) %>%
  ggplot(aes(x= reorder(word, freq), y=freq), fill=word) + geom_col() +  
  scale_fill_brewer(((palette = colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))))) + 
  coord_flip() + 
  labs(title = "Tweets #miprimeracoso 2016") + 
  labs(y = "Num. repeticiones") + 
  labs(x= NULL) + 
  theme_bw() 


##checar bag of words / lapplace coefficient / word to get /rex ex
