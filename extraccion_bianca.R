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
