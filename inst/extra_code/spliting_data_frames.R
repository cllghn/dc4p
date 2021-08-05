library(tidyverse)
df <- readr::read_csv(file = "data/Major_Contract_Awards.csv")

names <- df %>%
  pull(`Fiscal Year`) %>%
  unique() %>%
  paste0("_Major_Contract_Awards.csv")

out <- df %>% 
  group_split(`Fiscal Year`) 

purrr::map2(out, names, ~ write_csv(.x, file = paste0("data/", .y)))
