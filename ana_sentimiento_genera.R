
#No olvidar cargar paqueterias y diccionario

library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)

#Se crea un Dataframe con las pabras de diccionario que se encontraron en el texto de tweet
all_tweets

 
diccionario<- `13428_2015_700_MOESM1_ESM.(1)`
 
diccionario_fil <- diccionario %>% 
   filter(diccionario$ValenceMean != 5.35-6 )
   
  
  
  

tuits_diccionario <- 
  all_tweets %>%
  unnest_tokens(input = "text", output = "Word") %>%
  inner_join(diccionario, ., by = "Word") %>%
  mutate(Tipo = ifelse( ValenceMean > 5 , "Positiva", "Negativa")) 


#graficos

g <- tuits_diccionario %>%
  count(Word, Tipo, sort = TRUE) %>%
  ungroup()


tuits_diccionario %>%
  group_by(Tipo) %>%
  filter()
  #ungroup() %>%
  #mutate(word = reorder(word, n)) %>%
  ggplot(aes( Word, fill = Tipo )) +
  geom_histogram(show.legend = FALSE, stat = "count") +
  facet_wrap(~Tipo, scales = "free_y") +
  labs(x = "Analisis de sentimiento",
       y = NULL)
 