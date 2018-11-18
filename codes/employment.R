library(tidyverse)
library(readxl)
library(ggthemes)

national_ec <- read_excel("population/economic_national.xlsx") 

national_ec <- national_ec %>% .[-1,] %>% 
  gather(season, value, `2008 1/4`: `2017 4/4`)

ec_summarise_15_64 <- national_ec %>% 
  mutate(value = parse_double(value)) %>% 
  filter(연령계층별 == "15 - 64세") %>% 
  group_by(season, 성별) %>% 
  summarise(value = mean(value))

ec_summarise <- national_ec %>% 
  mutate(value = parse_double(value)) %>% 
  filter(연령계층별 == "20 - 29세") %>% 
  group_by(season, 성별) %>% 
  summarise(value = mean(value))

ggplot(ec_summarise) +
  geom_line(aes(season, value, color = 성별, group = 성별)) +
  scale_y_continuous(limits = c(50, 70)) +
  labs(title = "남성과 여성 고용률 (2008-2017 분기별)", subtitle = "20 - 29세", x = "기간", y = "고용률 (%)")


ggplot(ec_summarise_15_64) +
  geom_line(aes(season, value, color = 성별, group = 성별)) +
  geom_text(aes(x=season, y=value, label=value), position=position_dodge(0.3), angle=45) +
  scale_y_continuous(limits = c(50, 80)) +
  labs(title = "거제시 남성과 여성 고용률 (2008-2017 분기별)", subtitle = "15 - 64세(전국)", x = "기간", y = "고용률 (%)") +
  theme_economist()

write_csv(ec_summarise_15_64, "3장_8_남녀고용률.csv")
