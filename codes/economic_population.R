library(tidyverse)
library(readxl)
library(ggthemes)

economy_population <- read_excel("population/economic_population.xlsx")

economy_population %>% 
  gather(gender, value, `male`:`female`) %>% 
  filter(category == "고용률 (%)") %>% 
  ggplot() +
  geom_line(aes(season, value, color = gender)) +
  geom_text(aes(x=season, y=value, label=value)) +
  labs(x = "기간", y = "고용률 (%)", title = "거제시 남녀 간 고용률 변화")+
  theme_economist()
