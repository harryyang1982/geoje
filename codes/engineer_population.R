library(tidyverse)
library(readxl)
library(RColorBrewer)

population <- read_excel("datasets/shipbuilding_population.xlsx")

population_tidy <- population %>% 
  select(-계:-합계) %>% 
  gather(part, freq, 사무직:`기능직 협력사`) %>% 
  group_by(연도, 부문별, part) %>% 
  summarize(freq = mean(freq))
  


ggplot(population_tidy %>% filter(part == "기술직")) +
  geom_col(aes(x = 연도, y = freq, fill = 부문별), position = "dodge") +
  scale_fill_brewer(palette ="Set2") +
  scale_x_continuous(breaks = seq(2002, 2016, 1)) +
  scale_y_continuous(breaks = seq(0, 20000, 2500))
  
ggplot(population_tidy %>% filter(part == "기술직")) +
  geom_col(aes(x = 연도, y = freq, fill = 부문별), position = "fill") +
  geom_text(aes(x = 연도, y = freq, label = freq), position = position_fill(0.5)) +
  scale_x_continuous(breaks = seq(2002, 2016, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette ="Set2") +
  labs(title = "엔지니어 구성(2002~2016)", subtitle = "넓이는 비중, 숫자는 절대 숫자")


table_popu <- population_tidy %>% 
  filter(part == "기술직") %>% 
  spread(부문별, freq)

library(moonBook)
write_excel_csv(table_popu, "table_popu.csv")

k <- population_tidy %>% 
  spread(부문별, freq) %>% 
  mutate(rate = round(해양 / (조선 + 기타) * 100, 2)) %>% 
  filter(part == "기술직") %>% 
  select(rate)

k$rate
