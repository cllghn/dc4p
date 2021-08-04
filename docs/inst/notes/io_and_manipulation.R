# Day 2: I/O in R + Grammar of Data Manipulation

# Load all required packages
library(tidyverse)

# 1. Reading data --------------------------------------------------------------
file_path <- "data/un_comtrade_mongolia_commodities_exchanges_2018to2020.csv"
read_csv(file = file_path)
# What does the output tell you?

# Don't include column names:
read_csv(file = file_path, col_names = FALSE)

# Skip a row:
read_csv(file = file_path, col_names = FALSE, skip = 1)

un_comtrade <- read_csv(file = file_path)

# 2. Explore data in base R ----------------------------------------------------
head(un_comtrade)
str(un_comtrade)
# Who is part of this data set?
table(un_comtrade$rtTitle, un_comtrade$ptTitle)
# When do these transactions take place?
range(un_comtrade$period) #what is the minimum and maximum?
table(un_comtrade$rtTitle, un_comtrade$period)
# What types of transactions?
table(un_comtrade$rtTitle, un_comtrade$rgDesc)

# 3. Export out a table --------------------------------------------------------
my_df <- as.data.frame.matrix(table(un_comtrade$rtTitle, un_comtrade$rgDesc))
my_df[["country"]] <- rownames(my_df)
write_csv(x = my_df, file = "test.csv")

# 4. Intro to {dplyr} ----------------------------------------------------------
# See cheat sheet: https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf
# Subset data row-wise with filter()
filter(un_comtrade, rtTitle == "China")
filter(un_comtrade, rtTitle %in% c("China", "USA")) 
filter(un_comtrade, rtTitle %in% c("China", "USA"), yr == 2019)
filter(un_comtrade, rtTitle %in% c("China", "USA"), yr == 2019, rgDesc == "Import")
# Hard to see the TradeValue in the printout, too many columns...
# Better than sub-setting with row numbers (un_comtrade[1:5]), why?

# Piping %>%
un_comtrade %>%
  head(3)
# un_comtrade %>% head(3) is equivalent to head(un_comtrade, 3)
# The pipe operator takes the thing on the LHS and pipes it into the function call on the RHS
# "un_comtrade and then head"

# Subsetting columns
select(un_comtrade, rtTitle, ptTitle, rgDesc, TradeValue, yr)
# or
un_comtrade %>%
  select(rtTitle, ptTitle, rgDesc, TradeValue, yr)
# merge it all together
un_comtrade %>%
  filter(rtTitle %in% c("China", "USA"), yr == 2019, rgDesc == "Import") %>%
  select(rtTitle, ptTitle, rgDesc, TradeValue, yr)

# Add new variables with mutate() ----------------------------------------------
# TODO one line example of mutate... perhaps with text...

# I believe Mongolia imports lots from China. But how much more?
(china_export <- filter(un_comtrade,
                       yr == 2020, rtTitle == "China", rgDesc == "Export"))
(my_gap <- un_comtrade %>%
  filter(yr == 2020, rgDesc == "Export") %>%
  mutate(import_relative = TradeValue / china_export$TradeValue) %>%
  select(rtTitle, import_relative))
# Inspect results
summary(my_gap)
hist(my_gap$import_relative)
# Arrange values
my_gap %>%
  arrange(import_relative)
my_gap %>%
  arrange(desc(import_relative)) #desc() helper function
# Get top 10
my_gap %>%
  arrange(desc(import_relative)) %>%
  head(10)

# Exercise: Find the largest importer of Mongolian goods for 2020 in the data.
# Then create a new variable for imports relative to trade value of imports for the highest importer of goods.
un_comtrade %>%
  filter(rgDesc == "Import", yr == 2020) %>%
  select(rtTitle, TradeValue) %>%
  arrange(desc(TradeValue))
(china_import <- filter(un_comtrade,
                        yr == 2020, rtTitle == "China", rgDesc == "Import"))
(my_gap <- un_comtrade %>%
    filter(yr == 2020, rgDesc == "Import") %>%
    mutate(import_relative = TradeValue / china_import$TradeValue) %>%
    select(rtTitle, import_relative, TradeValue) %>%
    arrange(desc(import_relative)))

# Look at it
View(my_gap)

# Export it:
write_csv(x = my_gap, file = "importers_of_mogolian_goods_2020.csv")

# What if I do not like the name of variables on the CSV?
# Renaming with rename() -------------------------------------------------------
my_gap %>%
  rename(country = rtTitle,
         trade_value = TradeValue)

my_gap %>%
  rename(country = rtTitle,
         trade_value = TradeValue) %>%
  write_csv(file = "importers_of_mogolian_goods_2020.csv")


# Re-coding
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50128/Reexports-and-Reimports
un_comtrade %>%
  select(rgDesc) %>%
  table()

# Recode with case_when()! -----------------------------------------------------
# What to do about the "re-imports" and "re-exports"? 
# ?case_when
un_comtrade %>%
  mutate(clean_desc = case_when(rgDesc == "Re-Export" ~ "Export",
            rgDesc == "Re-Import" ~ "Import",
            TRUE ~ rgDesc # Keeps all present values the same
            )) %>%
  select(clean_desc) %>%
  table()

(un_comtrade <- un_comtrade %>%
  mutate(clean_desc = case_when(rgDesc == "Re-Export" ~ "Export",
                                rgDesc == "Re-Import" ~ "Import",
                                TRUE ~ rgDesc # Keeps all present values the same
  )))

# Re run analysis:
(china_import <- filter(un_comtrade,
                        yr == 2020, rtTitle == "China", rgDesc == "Import"))
(my_gap <- un_comtrade %>%
    filter(yr == 2020, rgDesc == "Import") %>%
    mutate(import_relative = TradeValue / china_import$TradeValue) %>%
    select(rtTitle, import_relative, TradeValue) %>%
    arrange(desc(import_relative)))

# The power of group_by() ------------------------------------------------------
un_comtrade %>%
  group_by(yr) # adds extra structure to your dataset, grouping information
# See top of the printout
# Multiple groupings:
un_comtrade %>%
  group_by(yr, clean_desc) 

# group_by() helper functions:
# Cheat sheet, see summary functions: https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf
#   - ?summarize() takes a grouped dataset computes requested summaries and returns rows for each group
#   - ?n() counts current size of a group
# Counting things up
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n())
# Similarly tally() counts groups
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  tally()
# What if we wanted to add the number of unique countries in each group?
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n(),
            n_countries = n_distinct(rtTitle))
# Statistical summaries: total trade value 
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n(),
            n_countries = n_distinct(rtTitle),
            total_ammount = sum(TradeValue))
# Statistical summaries: general descriptive statistics
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n(),
            n_countries = n_distinct(rtTitle),
            total_ammount = sum(TradeValue),
            max_ammount = max(TradeValue),
            min_ammount = min(TradeValue),
            avg_ammount = mean(TradeValue)
            )
# Grouped mutation
# Sometimes you don’t want to collapse the n rows for each group into one row.
# You want to keep your groups, but compute within them.
# For example, calculate the relative_trade for imports and exports, per country, based on the maximum value for that year.
un_comtrade %>%
  group_by(yr, clean_desc) %>% # Group by year and type of trade
  mutate(trade_relative= TradeValue / max(TradeValue)) %>%
  select(rtTitle, trade_relative, TradeValue) %>%
  arrange(desc(trade_relative))

# Putting it all together: Calculate China's trade delta from year to year, on exports and imports.
un_comtrade %>%
  filter(rtTitle == "China") %>%
  arrange(yr) %>%
  group_by(clean_desc, rtTitle) %>%
  mutate(trade_delta= TradeValue - lag(TradeValue)) %>%
  select(rtTitle, trade_delta, TradeValue, yr) %>%
  arrange(clean_desc, yr)

# Grand finale -----------------------------------------------------------------
# Calculate the import trade value changes from one year to another by country
(delta <- un_comtrade %>%
  filter(clean_desc == "Import") %>%
  select(yr, rtTitle, TradeValue) %>%
  group_by(rtTitle) %>%
  arrange(yr) %>%
  mutate(trade_delta = TradeValue - lag(TradeValue)) %>%
  arrange(rtTitle, yr, desc(trade_delta)) %>%
  filter(!is.na(trade_delta)))

write_csv(delta, file = "import_trade_yearly_changes.csv")
