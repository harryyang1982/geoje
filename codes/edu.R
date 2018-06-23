library(tidyverse)

education <- read_csv("datasets/education.csv",
                      locale = locale(encoding = "CP949"))

education_tidy <- education %>% 
  gather(year, value, `2007`:`2013_6`) %>% 
  mutate(category = ifelse(str_detect(value, "[a-zA-Z가-힣]"), value, NA)) %>% 
  fill(category) %>% 
  mutate(value = as.double(value)) %>% 
  na.omit(value) %>% 
  mutate(age = ifelse(`행정구역(시군)` %in% c("15~19세", "20~29세", "30세미만", "30~39세", "40~49세", "50~59세", "60세이상", "65세이상"), `행정구역(시군)`, NA),
         district = ifelse(`행정구역(시군)` %in% c("경남", "시지역", "군지역", "창원시", "마산시", "진주시", "진해시", "통영시", "사천시", "김해시", "밀양시", "거제시", "양산시", "의령군", "함안군", "창녕군", "고성군", "남해군", "하동군", "산청군", "함양군", "거창군", "합천군"), `행정구역(시군)`, NA),
         family_type = ifelse(`행정구역(시군)` %in% c("가구주", "배우자", "가구원"), `행정구역(시군)`, NA),
         job = ifelse(`행정구역(시군)` %in% c("관리직", "전문가준전문가", "사무직", "서비스판매직", "농림수산직", "기능원기계조작원", "단순노무직", "가정주부","기타"), `행정구역(시군)`, NA),
         gender = ifelse(`행정구역(시군)` %in% c("남자", "여자"), `행정구역(시군)`, NA),
         school = ifelse(`행정구역(시군)` %in% c("초졸", "중졸", "고졸", "대졸"), `행정구역(시군)`, NA)) %>% 
  select(-`행정구역(시군)`) %>% 
  rename(question = category) %>% 
  mutate(year = str_sub(year, 1L, 4L)) %>% 
  gather(category, subcategory, age:school) %>% 
  na.omit(subcategory)

edu_sum <- education_tidy %>% 
  group_by(question, category, subcategory) %>% 
  summarize(value = mean(value))

ggplot(edu_sum %>% filter(question == "보충 교육비 (천원)" & 
                            category == "district")) +
  geom_col(aes(x = reorder(subcategory, -value), y = value, fill = subcategory))
