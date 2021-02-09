library(readr)
library(dplyr)


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
