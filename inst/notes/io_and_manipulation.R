# Day 2: I/O in R + Grammar of Data Manipulation

# Load all required packages
library(tidyverse)

# 1. Reading data --------------------------------------------------------------
# Google "UN COMTRADE
file_path <- "data/un_comtrade_mongolia_commodities_exchanges_2018to2020.csv"
read_csv(file = file_path)
# What does the output tell you?

# Don't include column names:
read_csv(file = file_path, col_names = FALSE)

# Skip a row:
read_csv(file = file_path, col_names = FALSE, skip = 1)

# Clean column headers
# Install Janitor
install.packages("janitor")
?janitor

janitor::clean_names(read_csv(file = file_path))

# Assign it to an object
un_comtrade <- janitor::clean_names(read_csv(file = file_path))

# 2. Explore data in base R ----------------------------------------------------
head(un_comtrade)
?str
str(un_comtrade)
# Google: "UN COMTRADE CODE BOOK"
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50039/UN-Comtrade-Reference-Tables
# Who is part of this data set?
table(un_comtrade$rt_title, un_comtrade$pt_title) # What does this mean? Nations trading with Mongolia
# When do these transactions take place?
?range
range(un_comtrade$period) #what is the minimum and maximum?
table(un_comtrade$rt_title, un_comtrade$period)
# What types of transactions?
table(un_comtrade$rt_title, un_comtrade$rg_desc)

# 3. Export out a table --------------------------------------------------------
my_df <- as.data.frame.matrix(table(un_comtrade$rt_title, un_comtrade$rg_desc))
View(my_df) # Notice country is the index not a column
# Add a variable in named country using the row names
my_df[["country"]] <- rownames(my_df)
write_csv(x = my_df, file = "test.csv") # Go find the data

# 4. Intro to {dplyr} ----------------------------------------------------------
# Filter =======================================================================
# See cheat sheet: https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf
?filter
# Subset data row-wise with filter()
filter(un_comtrade, rt_title == "China")
# Multiple ?`%in%` example: 3 %in% 1:3 vs 3 %in% 1:2
filter(un_comtrade, rt_title %in% c("China", "USA")) 
filter(un_comtrade, rt_title %in% c("China", "USA"), yr == 2019)
filter(un_comtrade, rt_title %in% c("China", "USA"), yr == 2019, rg_desc == "Import")
# Better than sub-setting with row numbers (un_comtrade[1:5]), why?
# filter() is explicit, so there is less of a chance that you forget.
# Hard to see the trade_value in the printout, too many columns... what can we do?

# Piping =======================================================================
# First a detour Piping %>%
un_comtrade %>%
  head(3)
# un_comtrade %>% head(3) is equivalent to head(un_comtrade, 3)
# The pipe operator takes the thing on the LHS and pipes it into the function call on the RHS
# "un_comtrade and then head"

# Selecting ====================================================================
?select()
# Subsetting columns
select(.data = un_comtrade, rt_title, pt_title, rg_desc, trade_value, yr)
# or with pipes
un_comtrade %>%
  select(rt_title, pt_title, rg_desc, trade_value, yr) # select assumes the df
# merge it all together: filter + select!
un_comtrade %>%
  filter(rt_title %in% c("China", "USA"), yr == 2019, rg_desc == "Import") %>%
  select(rt_title, pt_title, rg_desc, trade_value, yr)

# Add new variables with mutate() ==============================================
# Create and modify columns:
?mutate
# Quick example: suppose you have the following data:
un_comtrade %>%
  select(rt_title, pt_title)
# Imagine you want a variable that includes both countries in the trade
# Like so:
paste(un_comtrade$rt_title, "trades with", un_comtrade$pt_title)
?paste
# Use mutate to add this to the dataset:
un_comtrade %>%
  select(rt_title, pt_title) %>%
  mutate(statement = paste(rt_title, "trades with", pt_title))

# Let's use mutate() for something more practical...
# Determine how much a country trades in comparison to another...
# I believe Mongolia imports lots from China. But how much more?
un_comtrade %>%
  # First filter
  filter(yr == 2020, rt_title == "China", rg_desc == "Export") %>%
  # Make smaller
  select(yr, rt_title, rg_desc, trade_value)
# Assign to an object
(china_export <- un_comtrade %>%
    # First filter
    filter(yr == 2020, rt_title == "China", rg_desc == "Export") %>%
    # Make smaller
    select(yr, rt_title, rg_desc, trade_value))
# Now let's create a new variable `import_relative` which represents each country trade releative to China in 2020
(my_gap <- un_comtrade %>% 
  # First filter
  filter(yr == 2020, rg_desc == "Export") %>%
  # Now mutate
  mutate(import_relative = trade_value / china_export$trade_value) %>%
  select(rt_title, import_relative))
# Inspect results
summary(my_gap)
hist(my_gap$import_relative)
# arrange() ====================================================================
?arrange
# Arrange values
my_gap %>%
  arrange(import_relative)
# In descending order
?desc
my_gap %>%
  arrange(desc(import_relative)) #desc() helper function
# Get top 10
my_gap %>%
  arrange(desc(import_relative)) %>%
  head(10)
# What countries does Mongolia import from?

# Exercise: Find the largest importer of Mongolian goods for 2020 in the data.
# Then create a new variable for imports relative to trade value of imports for the highest importer of goods.
un_comtrade %>%
  filter(rg_desc == "Import", yr == 2020) %>%
  select(rt_title, trade_value) %>%
  arrange(desc(trade_value))
(china_import <- filter(un_comtrade,
                        yr == 2020, rt_title == "China", rg_desc == "Import"))
(my_gap <- un_comtrade %>%
    filter(yr == 2020, rg_desc == "Import") %>%
    mutate(import_relative = trade_value / china_import$trade_value) %>%
    select(rt_title, import_relative, trade_value) %>%
    arrange(desc(import_relative)))

# Look at it
View(my_gap)

# Export it:
write_csv(x = my_gap, file = "importers_of_mogolian_goods_2020.csv")

# What if I do not like the name of variables on the CSV?
# Renaming with rename() =======================================================
?rename
my_gap %>%
  rename(country = rt_title,
         trade_value = trade_value)

my_gap %>%
  rename(country = rt_title,
         trade_value = trade_value) %>%
  write_csv(file = "importers_of_mogolian_goods_2020.csv")


# Re-coding: Often used in exploring a dataset
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50128/Reexports-and-Reimports
un_comtrade %>%
  select(rg_desc) %>%
  table()

# Recode with case_when()! -----------------------------------------------------
# What to do about the "re-imports" and "re-exports"? 
?case_when
un_comtrade %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
            rg_desc == "Re-Import" ~ "Import",
            TRUE ~ rg_desc # Keeps all present values the same
            )) %>%
  select(clean_desc) %>%
  table()
# Create new variable:
(un_comtrade <- un_comtrade %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc # Keeps all present values the same
  )))

# Re run analysis:
(china_import <- filter(un_comtrade,
                        yr == 2020, rt_title == "China", rg_desc == "Import"))
(my_gap <- un_comtrade %>%
    filter(yr == 2020, rg_desc == "Import") %>%
    mutate(import_relative = trade_value / china_import$trade_value) %>%
    select(rt_title, import_relative, trade_value) %>%
    arrange(desc(import_relative)))
# Did it change?

# The power of group_by() ======================================================
# We may want to explore our data in groups, such as by year, country, etc.
?group_by()
un_comtrade %>%
  group_by(yr) # adds extra structure to your dataset, grouping information
# See top of the printout

# Multiple groupings:
un_comtrade %>%
  group_by(yr, clean_desc) 
# See top of the printout

# group_by() helper functions:
# Cheat sheet, see summary functions: https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf
#   - ?summarize() takes a grouped dataset computes requested summaries and returns rows for each group
#   - ?n() counts current size of a group
# Counting things up
?summarize
?n
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n())
# Creates a new dataset from the summarized data
# Similarly tally() counts groups
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  tally()
# What if we wanted to add the number of unique countries in each group?
?n_distinct
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n(),
            n_countries = n_distinct(rt_title))
# Statistical summaries: total trade value by year and type
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n(),
            n_countries = n_distinct(rt_title),
            total_ammount = sum(trade_value))
# Statistical summaries: general descriptive statistics
un_comtrade %>%
  group_by(yr, clean_desc) %>%
  summarize(total = n(),
            n_countries = n_distinct(rt_title),
            total_ammount = sum(trade_value),
            max_ammount = max(trade_value),
            min_ammount = min(trade_value),
            avg_ammount = mean(trade_value)
            )
# Grouped mutation
# Sometimes you don’t want to collapse the n rows for each group into one row.
# You want to keep your groups, but compute within them.
# For example, calculate the relative_trade for imports and exports, per country, based on the maximum value for that year.
un_comtrade %>%
  group_by(yr, clean_desc) %>% # Group by year and type of trade
  mutate(trade_relative = trade_value / max(trade_value))
# Did it produce a new data set?

# Select to reduce the number of variables
un_comtrade %>%
  group_by(yr, clean_desc) %>% # Group by year and type of trade
  mutate(trade_relative = trade_value / max(trade_value)) %>%
  select(rt_title, trade_relative, trade_value)
# Now arrange to clean it up:
un_comtrade %>%
  group_by(yr, clean_desc) %>% # Group by year and type of trade
  mutate(trade_relative = trade_value / max(trade_value)) %>%
  select(rt_title, trade_relative, trade_value) %>%
  arrange(desc(trade_relative))

# Putting it all together: Calculate China's trade delta from year to year, on exports and imports.
un_comtrade %>%
  filter(rt_title == "China") %>%
  # Arrange data in chronological order
  arrange(yr) %>%
  # Group by type and Country reporting
  group_by(clean_desc, rt_title) %>%
  # calculate the delta: current year of trade, minus prior year, per category (import or export)
  mutate(trade_delta = trade_value - lag(trade_value)) %>% #lag()
  # reduce the noise
  select(rt_title, trade_delta, trade_value, yr) %>%
  # rearrange it
  arrange(clean_desc, yr)

# Grand finale -----------------------------------------------------------------
# Calculate the import from Mongolia trade value changes from one year to another by country
(delta <- un_comtrade %>%
  filter(clean_desc == "Import") %>%
  select(yr, rt_title, trade_value) %>%
  group_by(rt_title) %>%
  arrange(yr) %>%
  mutate(trade_delta = trade_value - lag(trade_value)) %>%
  arrange(rt_title, yr, desc(trade_delta)) %>%
  filter(!is.na(trade_delta)))

write_csv(delta, file = "import_trade_yearly_changes.csv")



# un_comtrade <- read_csv(file = "https://raw.githubusercontent.com/cjcallag/dc4p/main/data/un_comtrade_mongolia_commodities_exchanges_2018to2020.csv?token=AG4QS5FX5WQ4422OQAG4AG3BCVKV4")
# 
# 
# un_comtrade %>%
#   select(rt_title, rg_desc, trade_value) %>%
#   mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
#                                 rg_desc == "Re-Import" ~ "Import",
#                                 TRUE ~ rg_desc # Keeps all present values the same
#   )) %>%
#   group_by(rt_title, clean_desc) %>%
#   summarise(total = sum(trade_value)) %>%
#   pivot_wider(names_from = clean_desc, values_from = total)
# 
# un_comtrade %>%
#   select(rt_title, rg_desc, trade_value) %>%
#   mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
#                                 rg_desc == "Re-Import" ~ "Import",
#                                 TRUE ~ rg_desc # Keeps all present values the same
#   )) %>%
#   group_by(rt_title, clean_desc) %>%
#   summarise(total = sum(trade_value)) %>%
#   pivot_wider(names_from = clean_desc, values_from = total) %>%
#   mutate(defecit = Export - Import)
