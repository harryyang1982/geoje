library(tidyverse)
library(extrafont)
y2014 <- read_csv("datasets/foreigner2014.csv")
y2015 <- read_csv("datasets/foreigner_2015.csv")
y2016 <- read_csv("datasets/foreigner_2016.csv")
y2017 <- read_csv("datasets/foreigner_2017.csv")

y2014 <- y2014 %>% spread(구분, population)

y2014 <- y2014 %>% 
  rename(country = year) %>% 
  mutate(year = 2014)

y2015 <- y2015 %>% 
  rename(country = year) %>% 
  mutate(year = 2015)

y2016 <- y2016 %>% 
  rename(country = year) %>% 
  mutate(year = 2016)

y2017 <- y2017 %>% 
  mutate(year = 2017)

y_overall <- bind_rows(y2014, y2015, y2016, y2017) 

# ggplot(y_overall %>% filter(country != "계", 
#                             country != "소계")) +
#   geom_line(aes(x = year, y = 계, color = country))


y_spread <- y_overall %>% 
  gather(gender, population, 계:여) %>% 
  spread(country, population)

y_spread2 <- y_overall %>% 
  gather(gender, population, 계:여) %>% 
  spread(year, population) %>% 
  arrange(-`2015`, -`2016`, -`2014`, -`2017`) %>% 
  filter(country != "소계",
         country != "계",
         gender != "남",
         gender != "여") %>% 
  select(-gender)
 
write_excel_csv(y_spread2, "datasets/foreigner_yearly.csv")
write_csv(y_spread2, "datasets/foreigner_yearly.csv")

library(xlsx)
write.xlsx(y_spread2, "datasets/foreigner_yearly.xlsx")

y_count <- y_overall %>% 
  count(year)

y_spread3 <- y_overall %>% 
  gather(gender, population, 계:여) %>% 
  group_by(year, country, gender) %>% 
  summarize(population = mean(population, na.rm = TRUE)) %>% 
  spread(gender, population) %>% 
  mutate(성비 = 남 / 여) %>% 
  arrange(성비)

write.xlsx(y_spread3, "datasets/foreigner_gender.xlsx")
write_excel_csv(y_spread3, "datasets/foreigner_gender.csv")

y_spread4 <- y_spread3 %>% 
  ungroup() %>% 
  mutate(country = str_replace(country, "말레이지아", "말레이시아")) %>% 
  filter(country %in% c("미얀마", "베트남", "네팔", "말레이시아", "스리랑카", "우즈베키스탄", "인도", "인도네시아", "타이"))

fonttable()
theme_update(text=element_text(family="AppleMyungjo"))
ggplot(y_spread4) +
  geom_line(aes(x = year, y = 계, color = country)) +
  geom_point(aes(x = year, y = 계, color = country), size = 2) +
  scale_y_continuous(breaks=seq(0, 2000, 50)) +
  geom_text(aes(x = year, y = 계, label = 계), vjust = -0.5,
            family = "NanumGothic")
