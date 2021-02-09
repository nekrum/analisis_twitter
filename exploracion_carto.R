library(readr)


carto_data <- read_csv(
  "table_5d787653.csv",
  col_types = cols(
    cartodb_id = col_integer(),
    id = col_character(),
    follower_count = col_integer()
))

str(carto_data)

carto_data$text

