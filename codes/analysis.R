library(tidyverse)
library(ggthemes)

geoje <- read_rds("datasets/geoje_region.RDS") %>% 
  mutate(region_in = factor(region_in),
         region_out = factor(region_out),
         city_in = factor(city_in),
         city_out = factor(city_out),
         year = factor(year)) %>% 
  mutate(ageg = ifelse(age >= 49, "over_50s",
                       ifelse(age >= 39, "40s",
                              ifelse(age >= 29, "30s",
                                     ifelse(age >= 24, "late_20s",
                                            ifelse(age >= 19, "early_20s", "under_20"))))))

age_gender_out <- geoje %>% 
  group_by(year, region_in, ageg, gender) %>% 
  summarise(count = n())

total_gender <- geoje %>% 
  group_by(year, ageg, gender, in_out) %>% 
  summarise(count = n())


net_in_out <- total_gender %>% 
  spread(in_out, count) %>% 
  mutate(net_in_out = `in` - out) %>% 
  rename(net_in = `in`, net_out = out)

net_men <- net_in_out %>% 
  filter(gender == "남성") %>% 
  group_by(year) %>% 
  summarise(total = sum(net_in_out))

net_women <- net_in_out %>% 
  filter(gender == "여성") %>% 
  group_by(year) %>% 
  summarise(total = sum(net_in_out))

net_total <- net_men %>% 
  left_join(net_women, by = "year")


# analysis

net_in_out %>% 
  filter(gender == "여성") %>% 
  ggplot(aes(year, net_in_out, color = ageg, group = ageg)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) + coord_flip() +
  labs(title = "여성", subtitle = "20대 여성의 유출(거제시 전출 전체)")

net_in_out %>% 
  filter(gender == "여성") %>% 
  ggplot(aes(year, net_in_out, fill = ageg)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) + scale_y_continuous(breaks = seq(-450, 300, by = 50)) +
  geom_text(aes(label = net_in_out), position=position_dodge(0.3)) +
  labs(title = "여성", subtitle = "20대 여성의 유출(거제시 전출 전체)") +
  theme_economist()

net_in_out %>% 
  filter(gender == "남성") %>% 
  ggplot(aes(year, net_in_out, color = ageg, group = ageg)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) + coord_flip() +
  labs(title = "남성", subtitle = "취업으로 젊은 남성이 유입")

net_in_out %>% 
  filter(gender == "남성") %>% 
  ggplot(aes(year, net_in_out, fill = ageg)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = net_in_out), position=position_dodge(0.3)) +
  geom_hline(yintercept = 0) + scale_y_continuous(breaks = seq(-450, 1500, by = 50)) +
  labs(title = "남성", subtitle = "취업으로 젊은 남성이 유입", x = "연도", y = "순증감") +
  theme_economist()

ggplot(net_total) +
  geom_line(aes(year, total.x, group = 1), color = "blue") + geom_point(aes(year, total.x, group = 1), color = "blue") +
  geom_line(aes(year, total.y, group = 1), color = "red") + geom_point(aes(year, total.y, group = 1), color = "red") +
  geom_hline(yintercept = 0)

ggplot(net_men) +
  geom_col(aes(year, total, fill = total))
ggplot(net_women) +
  geom_col(aes(year, total, fill = total))

# 경남 외 전출

total_gender2 <- geoje %>% 
  filter(region_in != "경상남도" & in_out == "out" | region_out != "경상남도" & in_out == "in") %>% 
  group_by(year, ageg, gender, in_out) %>% 
  summarise(count = n())

net_in_out2 <- total_gender2 %>% 
  spread(in_out, count) %>% 
  mutate(net_in_out = `in` - out) %>% 
  rename(net_in = `in`, net_out = out)

net_in_out2 %>% 
  filter(gender == "여성") %>% 
  ggplot(aes(year, net_in_out, fill = ageg)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) + scale_y_continuous(breaks = seq(-450, 300, by = 50)) +
  geom_text(aes(label = net_in_out), position=position_dodge(0.3)) +
  labs(title = "여성", subtitle = "20대 여성의 유출(경남도 외로 전출)") +
  theme_economist()

# 경남 내 전출

total_gender3 <- geoje %>% 
  filter(region_in == "경상남도" & in_out == "out" | region_out == "경상남도" & in_out == "in") %>% 
  group_by(year, ageg, gender, in_out) %>% 
  summarise(count = n())

net_in_out3 <- total_gender3 %>% 
  spread(in_out, count) %>% 
  mutate(net_in_out = `in` - out) %>% 
  rename(net_in = `in`, net_out = out)

net_in_out3 %>% 
  filter(gender == "여성") %>% 
  ggplot(aes(year, net_in_out, fill = ageg)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) + scale_y_continuous(breaks = seq(-150, 300, by = 50), limits = c(-150, 150)) +
  geom_text(aes(label = net_in_out), position=position_dodge(0.5), angle = 45, hjust = 0.5) +
  labs(title = "여성", subtitle = "20대 여성의 유출 (경남도 내 전출)") +
  theme_economist()

# 어디로 가는가

where_out <- geoje %>% 
  filter(region_in != "경상남도" & in_out == "out", year == 2016) %>% 
  group_by(region_in, city_in, ageg, gender) %>% 
  summarise(count = n())

where_out %>% 
  filter(gender == "여성", count >= 5, ageg %in% c("early_20s", "late_20s")) %>% 
  mutate(region_city = str_c(region_in, city_in, sep = " ")) %>% 
  top_n(20) %>% 
  ggplot() +
  geom_col(aes(x = reorder(region_city, count), y = count, fill = ageg), position = "dodge") +
  geom_text(aes(x = reorder(region_city, count), y = count, label = count), position=position_dodge(0.3)) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
  labs(title = "20대 여성이 전출입하는 시군구", x = "시군구", y = "인원수") +
  theme_economist()

?top_n

where_out %>% 
  filter(gender == "남성", count >= 10, ageg %in% c("early_20s", "late_20s", "30s")) %>% 
  ggplot() +
  geom_col(aes(x = reorder(city_in, count), y = count, fill = ageg), position = "dodge") +
  coord_flip()
