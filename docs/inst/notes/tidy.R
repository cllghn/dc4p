# Day 2: Tidy data
library(tidyverse)
# Example from: r4ds
# Let's  take a look at three tables with the same data, organized differently:
table1
table2
table3
# While teh data is the same, not all representations are as easy to use.
# In this course we will use the tidy data framework:
#   - Each variable must have its own column
#   - Each observation its own row
#   - Each value its own cell

# In this example, only table1 is tidy.
# It’s the only representation where each column is a variable.

# Why ensure that your data is tidy? There are two main advantages:
#   - There’s a general advantage to picking one consistent way of storing data. 
#   If you have a consistent data structure, it’s easier to learn the tools that work with it because they have an underlying uniformity.
#   - There’s a specific advantage to placing variables in columns.
#   As you learned in mutate and summary functions, most built-in R functions work with vectors of values. 
#   That makes transforming tidy data feel particularly natural.

# For example: 
table1 %>%
  # Create new variable using cases and population vectors:
  mutate(rate = cases / population)

# UN COMTRADE Data -------------------------------------------------------------
# First read it:
file_path = "data/un_comtrade_mongolia_commodities_exchanges_2018to2020.csv"
un_comtrade <- read_csv(file = file_path) %>%
  janitor::clean_names()

# Goal: First, calculate the balance of trade between Mongolia and trading partners.
# To do so, we can subtract the imports (from Mongolia to partner) from the exports (from partner to Mongolia).
# Next, graph it!
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50136/Differences-between-Imports-and-Exports-Reporters-and-Partners-

# Explore the data:
un_comtrade

# Select a smaller number of variables:
un_comtrade %>%
  select(rt_title, rg_desc, trade_value)

# Remember we have to recode some values in the rg_desc variable
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc))

# Now aggregate the trade_value by the country and type of activity:
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value))

# In essence, I need to substract one type of descrition of trade from the other, but they are stacked.
# Enter, pivoting: You can use it when an observation is scattered accross multiple rows
# Look at tidy data example on https://r4ds.had.co.nz/tidy-data.html
# Figure 12.3
# Pivot wider ==================================================================
?pivot_wider

# Add this function to our chain of commands
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, # Which variable will be used to create new variables
              values_from = total) # which variable contains the data for these new columns

# At this point, we may want to calculate that trade balance:
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, # Which variable will be used to create new variables
              values_from = total) %>%
  mutate(trade_balance = Export - Import)

# Does this look right? No, a number minus NA is NA.
# We can fix this with some cleaning:
?if_else
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, # Which variable will be used to create new variables
              values_from = total) %>%
  mutate(
    Export = if_else(is.na(Export), 0, Export),
    Import = if_else(is.na(Import), 0, Import),
    trade_balance = Export - Import)

# Much better! All done. 
# Who has the biggest negative balance?
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, # Which variable will be used to create new variables
              values_from = total) %>%
  mutate(
    Export = if_else(is.na(Export), 0, Export),
    Import = if_else(is.na(Import), 0, Import),
    trade_balance = Export - Import) %>%
  arrange(desc(trade_balance))

# Having the data in this format is really useful for calculating descriptive statistics:
# Data set is still grouped
un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, # Which variable will be used to create new variables
              values_from = total) %>%
  mutate(
    Export = if_else(is.na(Export), 0, Export),
    Import = if_else(is.na(Import), 0, Import),
    trade_balance = Export - Import) %>%
  ungroup() %>%
  summary()

# Let's store the wide data:
wide_un_comtrade <- un_comtrade %>%
  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc)) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, # Which variable will be used to create new variables
              values_from = total) %>%
  mutate(
    Export = if_else(is.na(Export), 0, Export),
    Import = if_else(is.na(Import), 0, Import),
    trade_balance = Export - Import) %>%
  ungroup()


# Pivot Longer =================================================================
# While wide-data is tidy, sometimes we may want to collapse data so that the variables are combined into one categorical variable.
# Enter pivot_longer, which "lengthens" data by increasing the number of rown and decreasing the number of columns.
# The inverse of pivot_wider()
?pivot_longer()
wide_un_comtrade %>%
  pivot_longer(-rt_title,
               values_to = "trade_value",
               names_to = "trade_type")
# Creating a variable with categories is useful for filtering:
wide_un_comtrade %>%
  pivot_longer(-rt_title,
               values_to = "trade_value",
               names_to = "trade_type") %>%
  filter(trade_type == "trade_balance")

# Or for graphing:
un_viz <- wide_un_comtrade %>%
  pivot_longer(-rt_title,
               values_to = "trade_value",
               names_to = "trade_type") %>%
  filter(trade_type == "trade_balance") %>%
  arrange(desc(trade_value))

ggplot(data = un_viz) + # top 10
  geom_bar(aes(x = reorder(rt_title, -trade_value), y = trade_value),
           stat = "identity", fill = "skyblue") +
  scale_y_continuous(label = scales::dollar) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 8))+
  labs(x = "Country", y = "Trade Value")

ggplot(data = head(un_viz, 10)) + # top 10
  geom_bar(aes(x = reorder(rt_title, -trade_value), y = trade_value),
           stat = "identity", fill = "skyblue") +
  scale_y_continuous(label = scales::dollar) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 8)) +
  labs(x = "Country", y = "Trade Value")

ggplot(data = tail(un_viz, 10)) + # bottom 10
  geom_bar(aes(x = reorder(rt_title, -trade_value), y = trade_value),
           stat = "identity", fill = "skyblue") +
  scale_y_continuous(label = scales::dollar) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 8)) +
  labs(x = "Country", y = "Trade Value")

