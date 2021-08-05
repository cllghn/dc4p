# https://cran.r-project.org/web/packages/countrycode/countrycode.pdf
library(tidyverse)
countrycode::codelist %>%
  write_csv(file = "data/codelist.csv")
