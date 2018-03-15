library(readxl)
library(tidyverse)

y2010_raw <- read_csv("population/2010년.txt", col_names = F)

y2010 <- y2010_raw %>% 
  rename(region_in = X1,
         city_in = X2,
         district_in = X3,
         year_in = X4,
         month_in = X5,
         day_in = X6,
         region_out = X7,
         city_out = X8,
         district_out = X9,
         position = X11,
         age = X12,
         gender = X13,
         number_family = X14,
         total_move = X15,
         male_move = X16,
         female_move = X17
  ) %>% 
  filter(position == 1) %>% 
  mutate(age = parse_double(age),
         gender = ifelse(gender %in% c(9, 3), 1, 
                         ifelse(gender %in% c(0, 4), 2, gender)),
         city_in2 = str_c(region_in, city_in),
         district_in2 = str_c(region_in, city_in, district_in),
         city_out2 = str_c(region_out, city_out),
         district_out2 = str_c(region_out, city_out, district_out)) %>% 
  select(year_in:day_in, region_in, region_out,age:district_out2) %>% 
  rename(city_in = city_in2,
         city_out = city_out2,
         district_out = district_out2,
         district_in = district_in2)

y2010$region_in <- factor(y2010$region_in)
y2010$region_out <- factor(y2010$region_out)
y2010$gender <- factor(y2010$gender)
y2010$city_in <- factor(y2010$city_in)
y2010$city_out <- factor(y2010$city_out)
y2010$district_in <- factor(y2010$district_in)
y2010$district_out <- factor(y2010$district_out)


levels(y2010$gender) <- c("남성", "여성")
table(y2010$gender)

# 주소 정리

## 시도 정리

code <- read_csv("code_book_new.csv")

region_code_filter <- code %>% 
  distinct(region_code, .keep_all = T) %>% 
  filter(region_code %in% levels(y2010$region_in)) %>% 
  arrange(region_code) %>% 
  filter(region != "동해출장소" & region != "북부출장소")

region_code_filter$region_code

levels(y2010$region_in) <- region_code_filter$region
levels(y2010$region_in)

region_code_filter2 <- code %>% 
  distinct(region_code, .keep_all = T) %>% 
  filter(region_code %in% levels(y2010$region_out)) %>% 
  arrange(region_code) %>% 
  filter(region != "동해출장소" & region != "북부출장소")
levels(y2010$region_out) <- region_code_filter2$region
levels(y2010$region_out)

# 시군구 정리

city_code_filter <- code %>% 
  distinct(city_code, .keep_all = T) %>% 
  filter(city_code %in% levels(y2010$city_out)) %>% 
  arrange(city_code)

city_code_filter2 <- code %>% 
  distinct(city_code, .keep_all = T) %>% 
  filter(city_code %in% levels(y2010$city_in)) %>% 
  arrange(city_code)

levels(y2010$city_out) <- city_code_filter$city
levels(y2010$city_in) <- city_code_filter2$city

levels(y2010$city_out)
levels(y2010$city_in)

# 구역 정리

# district_code_filter <- code %>% 
#   distinct(district_code, .keep_all = T) %>% 
#   filter(district_code %in% levels(y2010$district_out)) %>% 
#   arrange(district_code)
# 
# district_code_filter2 <- code %>% 
#   distinct(district_code, .keep_all = T) %>% 
#   filter(district_code %in% levels(y2010$district_in)) %>% 
#   arrange(district_code)
# 
# code %>% 
#   distinct(district_code)


# 거제에서/거제로 진출/전입하는 시/도 단위

geoje_region_out_2010 <- y2010 %>% 
  filter(region_out == "경상남도" & city_out == "거제시" & city_in != "거제시")

geoje_region_in_2010 <- y2010 %>% 
  filter(region_in == "경상남도" & city_in == "거제시" & city_out != "거제시")

age_geoje_out <- geoje_region_out_2010 %>% 
  mutate(ageg = ifelse(age <= 18, "10s",
                       ifelse(age <= 29, "20s",
                              ifelse(age <= 39, "30s",
                                     ifelse(age <= 49, "40s",
                                            ifelse(age <= 59, "50s", "over_50s")))))) %>% 
  group_by(region_in, ageg) %>% 
  summarise(count = n())

age_geoje_in <- geoje_region_in_2010 %>% 
  mutate(ageg = ifelse(age <= 18, "10s",
                       ifelse(age <= 29, "20s",
                              ifelse(age <= 39, "30s",
                                     ifelse(age <= 49, "40s",
                                            ifelse(age <= 59, "50s", "over_50s")))))) %>% 
  group_by(region_out, ageg) %>% 
  summarise(count = n())

gender_geoje_out <- geoje_region_out_2010 %>% 
  group_by(region_in, gender) %>% 
  summarise(count = n())

gender_geoje_in <- geoje_region_in_2010 %>% 
  group_by(region_out, gender) %>% 
  summarise(count = n())

# 시군구로 본 거제 in/out

city_geoje_out <- geoje_region_out_2010 %>% 
  mutate(ageg = ifelse(age <= 18, "10s",
                       ifelse(age <= 29, "20s",
                              ifelse(age <= 39, "30s",
                                     ifelse(age <= 49, "40s",
                                            ifelse(age <= 59, "50s", "over_50s")))))) %>% 
  group_by(region_in, city_in, ageg, gender) %>% 
  summarise(count = n())

city_geoje_in <- geoje_region_in_2010 %>% 
  mutate(ageg = ifelse(age <= 18, "10s",
                       ifelse(age <= 29, "20s",
                              ifelse(age <= 39, "30s",
                                     ifelse(age <= 49, "40s",
                                            ifelse(age <= 59, "50s", "over_50s")))))) %>% 
  group_by(region_out, city_out, ageg, gender) %>% 
  summarise(count = n())

# binding

geoje_region_2010 <- bind_rows(geoje_region_in_2010 %>% mutate(in_out = "in"),
                               geoje_region_out_2010 %>% mutate(in_out = "out"))
geoje_city_2010 <- bind_rows(city_geoje_in %>% mutate(in_out = "in"),
                             city_geoje_out %>% mutate(in_out = "out"))

write_rds(geoje_region_2010, "geoje_region_2010.RDS")
write_rds(geoje_city_2010, "geoje_city_2010.RDS")
