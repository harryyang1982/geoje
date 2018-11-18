library(tidyverse)
library(ggthemes)
library(readxl)

APT_rate <- read_excel("datasets/APT_rate.xlsx")

APT_rate %>% 
  gather(year, value, `2003`:`2017`) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(group = 1) +
  geom_text(aes(label = value), vjust = -1) +
  theme_economist() +
  labs(title = "거제 아파트 가격 추이", subtitle = "2003~2017, 출처: 통계청 e-지방지표")

APT_rate %>% 
  gather(year, value, `2003`:`2017`) %>% 
  write_csv("3장_1.csv")
