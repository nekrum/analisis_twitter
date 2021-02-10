library(rtweet)


rt <- search_tweets(
  "#rstats", n = 18000, include_rts = FALSE
)
names(rt)
users_data(rt)

# No data
# miprimeracoso2016tri1 <- search_fullarchive(
#   q = "#miprimeracoso",
#   env_name = 'buscatodo',
#   fromDate = "201601010000", toDate = "201603010000"
# )


miprimeracoso2016tri1 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201604010000", toDate = "201603010000"
)

miprimeracoso2016 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201601010000", toDate = "201612310000"
)

range(miprimeracoso2016$created_at)

miprimeracoso2017 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201701010000", toDate = "201712310000"
)
range(miprimeracoso2017$created_at)

miprimeracoso2018 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201801010000", toDate = "201812310000"
)
range(miprimeracoso2018$created_at)

miprimeracoso2019 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "201901010000", toDate = "201912310000"
)
range(miprimeracoso2019$created_at)

miprimeracoso2020 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "202001010000", toDate = "202012310000"
)
range(miprimeracoso2020$created_at)

miprimeracoso2021 <- search_fullarchive(
  q = "#miprimeracoso",
  env_name = 'buscatodo',
  fromDate = "202101010000", toDate = "202101310000"
)
range(miprimeracoso2021$created_at)
