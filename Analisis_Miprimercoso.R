library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)

rt <- search_tweets(
  "#rstats", n = 18000, include_rts = FALSE
)
names(rt)
users_data(rt)

# empty
# miprimeracoso2016_0101_0229
# miprimeracoso2016_0301_0331
# miprimeracoso2016_0401_0410
# miprimeracoso2016_0411_0420
# miprimeracoso2016_0421_0422
miprimeracoso2016_04231700_04231800 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'buscatodo',
  fromDate = "201604231700", toDate = "201604231800"
)
saveRDS(miprimeracoso2016_04231700_04231800, "miprimeracoso2016_04231700_04231800.rds")

miprimeracoso2016_04231800_04231900 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'buscatodo',
  fromDate = "201604231800", toDate = "201604231900"
)
saveRDS(miprimeracoso2016_04231800_04231900, "miprimeracoso2016_04231800_04231900.rds")

miprimeracoso2016_04231900_04231940 <- search_fullarchive(
  q = "#miprimeracoso", n = 500,
  env_name = 'buscatodo',
  fromDate = "201604231900", toDate = "201604231940",
)
saveRDS(miprimeracoso2016_04231900_04231940, "miprimeracoso2016_04231900_04231940.rds")

miprimeracoso2016_04231900_04232000 <- unique(rbind(miprimeracoso2016_04231900_04232000, miprimeracoso2016_04231900_04231940))
saveRDS(miprimeracoso2016_04231900_04232000, "miprimeracoso2016_04231900_04232000.rds")


miprimeracoso2016_1203_1209 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201601010000", toDate = "201612100000"
)

miprimeracoso2016 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201601010000", toDate = "201612310000"
)
saveRDS(miprimeracoso2016, "miprimeracoso2016.rds")
range(miprimeracoso2016$created_at)

miprimeracoso20171008_1115 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201612310000", toDate = "201711160000"
)
saveRDS(miprimeracoso20171008_1115, "miprimeracoso20171008_1115.rds")

miprimeracoso2017 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201601010000", toDate = "201712310000"
)
saveRDS(miprimeracoso2017, "miprimeracoso2017.rds")
range(miprimeracoso2017$created_at)

miprimeracoso2018 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201801010000", toDate = "201812310000"
)
saveRDS(miprimeracoso2018, "miprimeracoso2018.rds")
range(miprimeracoso2018$created_at)

miprimeracoso2019 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201901010000", toDate = "201912310000"
)
saveRDS(miprimeracoso2019, "miprimeracoso2019.rds")
range(miprimeracoso2019$created_at)

miprimeracoso2020 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "202001010000", toDate = "202012310000"
)
saveRDS(miprimeracoso2020, "miprimeracoso2020.rds")
range(miprimeracoso2020$created_at)

miprimeracoso2021 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "202101010000", toDate = "202101310000"
)
saveRDS(miprimeracoso2021, "miprimeracoso2021.rds")
range(miprimeracoso2021$created_at)


files.rds <- list.files(path = ".", pattern = "*.rds")

all.tweets <- lapply(files.rds, readRDS) %>% bind_rows() %>% distinct()

all.tweets %>%
  # filter(year(created_at) == '2019', month(created_at) %in% 10, day(created_at) %in% 25:30) %>%
  mutate(day_tw = date(created_at)) %>%
  group_by(day_tw) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = day_tw, y=n)) +
  geom_line()

saveRDS(all.tweets, "all_tweets.rds")
Twetts_mi_primer_acoso <- readRDS(file.choose())

##Creacion y filtrado del texto
