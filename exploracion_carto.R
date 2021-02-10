library(readr)
library(dplyr)
library(lubridate)


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



