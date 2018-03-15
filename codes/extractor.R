library(tidyverse)

files <- dir("datasets", pattern = "\\.RDS", full.names = T)
files <- files[17:length(files)]

out <- vector("list", length(files))
for (i in seq_along(files)) {
  out[[i]] <- read_rds(files[[i]])
}

total_geoje <- map_df(out, bind_rows) %>% 
  rename(year = year_in) %>% 
  select(-month_in, -day_in)

write_rds(total_geoje, "datasets/geoje_region.RDS")
