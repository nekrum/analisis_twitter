library(rtweet)
library(dplyr)
library(ggplot2)
library(lubridate)

miprimeracoso2016_04232000_04232020 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'historicotweet',
  fromDate = "201604232000", toDate = "201604232020"
)
saveRDS(miprimeracoso2016_04232000_04232020, "miprimeracoso2016_04232000_04232020.rds")

#Continuacion

miprimeracoso2016_04232020_04232040 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'historicotweet',
  fromDate = "201604232020", toDate = "201604232040"
)
saveRDS(miprimeracoso2016_04232020_04232040, "miprimeracoso2016_04232000_04232020.rds")

miprimeracoso2016_04232040_04232100 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'historicotweet',
  fromDate = "201604232040", toDate = "201604232100"
)
saveRDS(miprimeracoso2016_04232040_04232100, "miprimeracoso2016_04232040_04232100.rds")

miprimeracoso2016_04232100_04232120 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'historicotweet',
  fromDate = "201604232100", toDate = "201604232120"
)
saveRDS(miprimeracoso2016_04232100_04232120, "miprimeracoso2016_04232100_04232120.rds")

miprimeracoso2016_04232120_04232140 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'historicotweet',
  fromDate = "201604232120", toDate = "201604232140"
)
saveRDS(miprimeracoso2016_04232120_04232140, "miprimeracoso2016_04232120_04232140.rds")

miprimeracoso2016_04232140_04232200 <- search_fullarchive(
  q = "#miprimeracoso",n = 500,
  env_name = 'historicotweet',
  fromDate = "201604232140", toDate = "201604232200"
)
saveRDS(miprimeracoso2016_04232140_04232200, "miprimeracoso2016_04232140_0423222.rds")






