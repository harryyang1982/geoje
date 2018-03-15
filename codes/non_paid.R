library(tidyverse)
library(readxl)

apartment <- read_excel("datasets/APT_rate.xlsx")

apartment <- apartment %>% 
  gather(year, value, `2003`:`2017`)

ggplot(apartment) +
  geom_line(aes(year, value, group = 1)) +
  ggthemes::theme_economist_white() +
  labs(title = "거제 아파트 가격 추이", subtitle = "2003~2017, 출처: 통계청 e-지방지표",
       xlab = "연도", ylab = "지수(100을 기준으로)")


non_paid <- read_excel("datasets/geoje_under.xlsx")

# non_paid[1,2] <- "전국"

names(non_paid)

non_paid %>% 
  mutate(`월(Monthly)` = parse_date(non_paid$`월(Monthly)`, format = "%Y-%m")) %>% 
  filter(구분 == '경남') %>% 
  ggplot() +
  geom_line(aes(`월(Monthly)`, 호, group =1 )) +
  labs(title = "거제 미분양 아파트 현황", subtitle = "준공후 미분양 현황 2010~2018",
       xlab = "연도/월", ylab = "호수") +
  scale_x_date(date_breaks = "1 year") +
  ggthemes::theme_economist_white()


price_rate <- read_excel("datasets/monthly_rate.xlsx")

month_price <- seq(as.Date("2010-01-01"), as.Date("2018-01-01"), "months")

price_rate$`지 역` <- c("경남전체", "거제")
price_rate$X__1 <- NULL

price_rate <- price_rate %>% 
  gather(month, rate, `2010년 01월`:`2018년 01월`)

price_rate <- price_rate %>% 
  mutate(month = rep(month_price, each = 2))
str(price_rate)

price_rate %>% 
  filter(month >= "2013-01-01") %>% 
  ggplot() + 
  geom_line(aes(month, rate, group = `지 역`, color = `지 역`)) +
  scale_x_date(date_breaks = "1 year") +
  ggthemes::theme_economist() +
  labs(title = "지가지수 변동", subtitle = "경남전체와 거제 비교(2013~2018)", ylab = "지수", xlab = "월")
