library(tidyverse)
library(readxl)

code_book <- read_excel("population/code_book.xlsx")

code_book_region <- code_book %>% 
  mutate(region_code = parse_character(code) %>% str_sub(1,2),
         city_code = parse_character(code) %>% str_sub(1,5),
         district_code = code)

write_csv(code_book_region, "code_book_new.csv")
