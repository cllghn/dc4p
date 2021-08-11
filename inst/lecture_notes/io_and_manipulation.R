# Review: I/O in R + Grammar of Data Manipulation

# Load all required packages
library(tidyverse)

# 1. Reading data --------------------------------------------------------------
# UN COMTRADE: https://comtrade.un.org/
file_path <- "data/un_comtrade_mongolia_commodities_exchanges_2018to2020.csv"
read_csv(file = file_path)
# What does the output tell you?

# Clean column headers
# Install Janitor
# install.packages("janitor")
# If you don't want to load the whole package, use the package name + colons (2)
# For example - janitor::
?janitor
# Clean variable names:
janitor::clean_names(read_csv(file = file_path))

# Assign it to an object:
un_comtrade <- janitor::clean_names(read_csv(file = file_path))

# 2. Explore data in base R ----------------------------------------------------
head(un_comtrade)
# Google: "UN COMTRADE CODE BOOK"
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50039/UN-Comtrade-Reference-Tables
# Who is part of this data set?
table(un_comtrade$rt_title) 

# 3. Intro to {dplyr} ----------------------------------------------------------
# Piping =======================================================================
# un_comtrade %>% head(3) is equivalent to head(un_comtrade, 3)
# The pipe operator takes the thing on the LHS and pipes it into the function call on the RHS
# "un_comtrade and then head"
un_comtrade %>%
  head(3)

# Filter =======================================================================
# See cheat sheet: https://raw.githubusercontent.com/rstudio/cheatsheets/master/data-transformation.pdf
?filter
# Subset data row-wise with filter()
# Only return variables with "China" as the rt_title (reporting partner):
un_comtrade %>%
  filter(rt_title == "China")
# Multiple ?`%in%` example: 3 %in% 1:3 vs 3 %in% 1:2
un_comtrade %>%
  filter(rt_title %in% c("China", "USA")) 
# Multiple filters, for example by rt_title and by year:
un_comtrade %>% 
  filter(rt_title %in% c("China", "USA"), yr == 2019)
un_comtrade %>%
  filter(rt_title %in% c("China", "USA"), yr == 2019, rg_desc == "Import")

# Selecting ====================================================================
# Subsetting columns
?select()
# Subsetting rt_title, pt_title, rg_desc, trade_value and yr columns
un_comtrade %>%
  select(rt_title, pt_title, rg_desc, trade_value, yr)

# Merge functions togethertogether: filter + select
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
