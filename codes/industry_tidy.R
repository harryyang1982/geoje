library(tidyverse)
library(ggthemes)


industry <- read_csv("datasets/industry.csv", locale = locale(encoding = "CP949"))

job_category <- industry_tidy %>% 
  select(category=index) %>% 
  na.omit() %>% 
  unique() %>% 
  filter(category != "여자 (명)", 
         category != "남자 (명)",
         category != "종사자수 (명)",
         category != "사업체수 (개소)",
         category != "여성대표자 사업체수 (개소)") %>% 
  unlist() %>% 
  unname()

industry_tidy <- industry %>% 
  gather(year, freq, `2009`:`2016_46`) %>% 
  mutate(index = ifelse(str_detect(freq, "[a-zA-Z가-힣]"), freq, NA)) %>% 
  mutate(freq = ifelse(str_detect(freq, "[0-9]"), freq, NA)) %>% 
  fill(index) %>% 
  mutate(category = ifelse(index %in% job_category, index, NA)) %>% 
  fill(category) %>% 
  na.omit(freq) %>% 
  filter(index != "여자 (명)",
         index != "남자 (명)",
         index != "여성대표자 사업체수 (개소)") %>% 
  mutate(year = str_sub(year, 1L, 4L)) %>% 
  mutate(freq = as.integer(freq)) %>% 
  select(-`읍면동별(1)`)


in_tidy <- industry_tidy %>% 
  filter(category != "합계") %>% 
  group_by(year, index, category) %>% 
  summarize(freq = mean(freq)) %>% 
  spread(index, freq) %>% 
  rename(company = "사업체수 (개소)",
         population = "종사자수 (명)") %>% 
  left_join(in_tidy %>% 
              group_by(year) %>% 
              summarize(s_company = sum(company),
                        s_population = sum(population))) %>% 
  mutate(pct_company = company / s_company,
         pct_population = population / s_population)


ggplot(in_tidy %>% filter(pct_population >= 0.05), aes(x = year, y = pct_population)) +
  geom_line(aes(color = category, group = category)) +
  geom_point(aes(color = category)) +
  geom_text(aes(label = round(pct_population * 100, 1), color = category), family="NanumGothic", 
            vjust = +1) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.05)) +
  theme_economist()

# library(extrafont)
# font_import()

in_tidy %>% 
  select(year, category, pct_population) %>% 
  mutate(pct_population = pct_population * 100) %>% 
  write_csv("1장_graph_2.csv")

in_tidy %>% 
  select(year, category, pct_population) %>% 
  mutate(pct_population = pct_population * 100) -> graph_2
graph_2
