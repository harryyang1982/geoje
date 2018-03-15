library(tidyverse)
library(ggmap)

?get_navermap()

get_navermap(geoje$city_in)
?get_googlemap

city_map <- geoje %>% 
  filter(ageg %in% c("early_20s"), gender == "여성", city_in != "거제시", region_in %in% c("서울특별시", "인천광역시", "경기도"))

city_code <- geocode(as.character(city_map$city_in))

ggmap(city_code)

map <- city_map %>% 
  bind_cols(city_code)

ggmap(map)

get_map()

map <- city_code %>% 
  filter(!is.na(lat)) %>% 
  get_map()

ggmap(map)
