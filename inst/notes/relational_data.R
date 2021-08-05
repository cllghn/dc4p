# Day 2: Relational Data

library(tidyverse)

# What happens when data arrives in many pieces? Rarely do we get data in a single table.
# Multiple tables of data = relational data b/c the relations between these data set not the data matters.

# Example 1. A school database of students and classes
# First table includes students and student id
people <- data.frame(pid = c(1, 2, 3, 4),
                     name = c("Mike", "Rob", "Chris", "Mike"),
                     lname = c("Aspland", "Schroeder", "Callaghan", "Stevens"),
                     gender = c("Male", "Male", "Male", "Male"),
                     status = c("Enrolled", "Enrolled", "Enrolled",
                                "Enrolled"))
# Second table is enrollement in a year
events_2020 <- data.frame(cid = c("A1", "A1", "A2", "A3", "A3", "A3", "A3"),
                          class = c("math", "math", "english", "physics", 
                                    "physics", "physics", "physics"),
                          pid = c(1, 2, 2, 1, 2, 3, 4),
                          year = 2020)
# Third table is enrollment in a year
events_2021 <- data.frame(cid = c("A1", "A1", "A2", "A2", "A2"),
                          class = c("math", "math", "english", "english",
                                    "english"),
                          pid = c(3, 4, 2, 3, 4),
                          year = 2020)
# Why lay out data this way?
# Detail: 2 Mikes
people %>%
  mutate(full_name = str_c(name, ", ", lname))

# Reporting
events_2020 %>%
  group_by(cid) %>%
  summarise(students = n())

# Here we will work with two kinds of combinations, binds and joins.
#   - Bind: joining based on rows (row-binding) or column (column-binding).
#       When row binding you must consider the variables of both tables.
#       When column binding, ensure the row are aligned. This method is not recommended!
#   - Joins: Combining data using a designated variable as a key. That is we join one table with another if they match on the key.
  
# Binds ------------------------------------------------------------------------
# First let's proceed to load two data sets
mca_2000 <- read_csv(file = "data/2000_Major_Contract_Awards.csv")
mca_2001 <- read_csv(file = "data/2001_Major_Contract_Awards.csv")

# Inspect them:
dim(mca_2000)
dim(mca_2001)
names(mca_2000)
names(mca_2001)
# Are the names the same?
names(mca_2000) == names(mca_2001)

# Then row bind them
mca_2000_2001 <- rbind(mca_2000, mca_2001)
# Inspect them:
dim(mca_2000_2001)
mca_2000_2001 %>%
  # First group
  group_by(`Fiscal Year`) %>%
  # Then count
  summarize(n = n()) # alternative table(mca_2000_2001$`Fiscal Year`)


# Exercise 1. Now your turn, read in all the remaining years (2002 to 2021) and bind them into a data.frame named mca, we will be using this for the rest of the class today
# Only read the CSV files labeled *_Major_Contract_Awards.csv...
# Massive cheat:
# names <- list.files(path = "data/",
#                     pattern = "*_Major_Contract_Awards.csv$",
#                     full.names = TRUE)
# mca <- purrr::map_df(names, read_csv)

# Inspect the data....
dim(mca)

# Joins ------------------------------------------------------------------------
# Let's load a new data set:
(codelist <- read_csv(file = "data/codelist.csv"))

# Inspect the data....
dim(codelist)
names(codelist)
head(codelist)


# We will try to join these datasets later, first identify the variable in either that may be useful in joining these data sets...

# Exercise 2.  Since the codelist data is rather extensive, let's reduce it down to some relevant variables; namely, the key (iso2c), continent, iso.name.en, region, and iso3
# Answer:
(sub_codelist <- codelist %>%
    select(iso2c, iso3c, iso.name.en, continent, region))

# Goal, join data. Work our way to a breakdown of investment by continent and then region.
# left_join() ==================================================================
# ?left_join
# Start with mca data
mca %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c"))
# Did it join? Read tibble dimensions

# Reorganize variables to test that:
mca %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso.name.en,
         everything()) # Selects all variables

# Did the merging process work? NA will happen when a merge is unsuccessful
mca %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso.name.en,
         everything()) %>%
  filter(is.na(iso.name.en)) %>%
  group_by(`Supplier Country`, `Supplier Country Code`, iso.name.en) %>%
  summarize(n = n()) 

# Trust but verify, inspect the dataset
mca %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso.name.en,
         everything()) %>%
  # filter(is.na(iso.name.en)) %>%
  group_by(`Supplier Country`, `Supplier Country Code`, iso.name.en) %>%
  summarize(n = n()) %>%
  View()

# Recode and remove!
# Central Africa = CF
# Congo, Democrat = CR
# Kosovo = Does not have ISO
# MeOthersico remove
# Namibia = "NA"
# Netherlands = NL
# Serbia = RS
# Remove St Marteen
# Timor-Leste = TL
# West Bank and Gaza = PS
# Remove World
# Yemen = YE

# Recoding
mca %>%
  # Filter out noise
  filter(!`Supplier Country` %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(`Supplier Country Code` = case_when(`Supplier Country` == "Central Africa" ~ "CF",
                                             `Supplier Country` == "Congo, Democrat" ~ "CR",
                                             `Supplier Country` == "Namibia" ~ "NA",
                                             `Supplier Country` == "Netherlands Ant" ~ "NL",
                                             `Supplier Country` == "Serbia" ~ "RS",
                                             `Supplier Country` == "Timor-Leste" ~ "TL",
                                             `Supplier Country` == "West Bank and G" ~ "PS",
                                             `Supplier Country` == "Yemen, Republic" ~ "YE",
                                             TRUE ~`Supplier Country Code`
                                             )) %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso.name.en,
        everything()) %>%
  filter(is.na(iso.name.en)) %>%
  group_by(`Supplier Country`, `Supplier Country Code`, iso.name.en) %>%
  summarize(n = n()) 

# Error in the codelist read
sub_codelist <- sub_codelist %>%
  mutate(iso2c = if_else(iso.name.en == "Namibia", "NA", iso2c))

#Try again
mca %>%
  # Filter out noise
  filter(!`Supplier Country` %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(`Supplier Country Code` = case_when(`Supplier Country` == "Central Africa" ~ "CF",
                                             `Supplier Country` == "Congo, Democrat" ~ "CR",
                                             `Supplier Country` == "Namibia" ~ "NA",
                                             `Supplier Country` == "Netherlands Ant" ~ "NL",
                                             `Supplier Country` == "Serbia" ~ "RS",
                                             `Supplier Country` == "Timor-Leste" ~ "TL",
                                             `Supplier Country` == "West Bank and G" ~ "PS",
                                             `Supplier Country` == "Yemen, Republic" ~ "YE",
                                             TRUE ~`Supplier Country Code`
  )) %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso.name.en,
         everything()) %>%
  filter(is.na(iso.name.en)) %>%
  group_by(`Supplier Country`, `Supplier Country Code`, iso.name.en) %>%
  summarize(n = n()) 
# All done!

(joined <- mca %>%
  # Filter out noise
  filter(!`Supplier Country` %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(`Supplier Country Code` = case_when(`Supplier Country` == "Central Africa" ~ "CF",
                                             `Supplier Country` == "Congo, Democrat" ~ "CR",
                                             `Supplier Country` == "Namibia" ~ "NA",
                                             `Supplier Country` == "Netherlands Ant" ~ "NL",
                                             `Supplier Country` == "Serbia" ~ "RS",
                                             `Supplier Country` == "Timor-Leste" ~ "TL",
                                             `Supplier Country` == "West Bank and G" ~ "PS",
                                             `Supplier Country` == "Yemen, Republic" ~ "YE",
                                             TRUE ~`Supplier Country Code`
  )) %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso.name.en,
         everything()))


# Exercise 3. With this new data set, let's calculate the total in contract awarded by continent in 2020
(total_by_contient <- joined %>%
  filter(`Fiscal Year` == 2020) %>% # First filter
  group_by(continent) %>% # Next group
  summarise(total = sum(`Total Contract Amount (USD)`)))

# Let's graph the results
ggplot() + 
  geom_col(data = total_by_contient, aes(x = continent, y = total))
# Let's clean it up
ggplot() + 
  geom_col(data = total_by_contient, aes(x = continent, y = total), fill = "darkgreen") +
  # Change the theme:
  theme_bw() +
  # Clean up the axis and add content
  labs(x = "Continent", y = "Ammount Supplied in Contracts",
       title = "Total Contract Amount by Continent in 2020",
       subtitle = "Amounts in USD",
       caption = "Data from World Bank") +
  # clean up the y axis
  scale_y_continuous(labels = scales::comma) +
  # tilt the y axis a little + change size
  theme(axis.text.y = element_text(angle = 45, vjust = 0.5, size = 8))

# Bonus:
(total_by_contient_country <- joined %>%
    filter(`Fiscal Year` == 2020) %>%
    group_by(continent, `Supplier Country`, `Supplier Country Code`) %>%
    summarize(total = sum(`Total Contract Amount (USD)`)))

ggplot() +
  geom_bar(data = filter(total_by_contient_country, continent == "Asia"), 
           aes(y = total, x = reorder(`Supplier Country`, -total)), # add ?reorder()
           stat = "identity", fill = "skyblue", position = "dodge") +
  theme_minimal() +
  # Clean up the axis and add content
  labs(x = NULL, y = NULL,
       title = "Total Contract Amount by Country for the Asian Continent in 2020",
       subtitle = "Amounts in USD",
       caption = "Data from World Bank") +
  scale_y_continuous(labels = scales::comma) +
  # tilt the y axis a little + change size
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
        axis.text.y = element_text(angle = 45, vjust = 0.5, size = 8))

# right_join() =================================================================
# Back to joins: https://r4ds.had.co.nz/relational-data.html#understanding-joins
# There are multiple types of joins,

# Say we want to calculate the percent of GDP awarded in contracts for 2020
# First read new data:
gdp <- read_csv(file = "data/wb_gdp_2000to2020.csv")
# Inspect it:
head(gdp) 
# tidy the data:
(gdp <- gdp %>%
  # First pivot
  select(-`Series Name`, -`Series Code`) %>%
  pivot_longer(!c("Country Code", "Country Name"), names_to = "year", values_to = "gdp") %>%
  # Now mutate: https://github.com/rstudio/cheatsheets/blob/master/strings.pdf
  mutate(year = str_replace(year, "[:space:].*+", ""), # regex is space and everything after
         year = as.numeric(year)))

# select(`Country Name`, `Country Code`, `2020 [YR2020]`))
# What keys do we have between mca and gdp? None? 
# We need to join the sub_contrycode data set to use the iso3 keys, then merge gdp

(joined <- mca %>%
  # Filter out noise
  filter(!`Supplier Country` %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(`Supplier Country Code` = case_when(`Supplier Country` == "Central Africa" ~ "CF",
                                             `Supplier Country` == "Congo, Democrat" ~ "CR",
                                             `Supplier Country` == "Namibia" ~ "NA",
                                             `Supplier Country` == "Netherlands Ant" ~ "NL",
                                             `Supplier Country` == "Serbia" ~ "RS",
                                             `Supplier Country` == "Timor-Leste" ~ "TL",
                                             `Supplier Country` == "West Bank and G" ~ "PS",
                                             `Supplier Country` == "Yemen, Republic" ~ "YE",
                                             TRUE ~`Supplier Country Code`)) %>%
  left_join(sub_codelist, by = c("Supplier Country Code" = "iso2c")) %>%
  select(`Supplier Country`, `Supplier Country Code`, iso3c, iso.name.en,
         everything()))

# More editing to get 2000 numbers per country:
joined <- joined %>%
  filter(`Fiscal Year` == 2020) %>%
  group_by(`Supplier Country`, iso3c) %>%
  summarise(total = sum(`Total Contract Amount (USD)`))
# Now join them:
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  View()
# How many countries in the World Bank GDP data not in contracts?

joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(`Supplier Country`)) %>% # Not NAs
  View()
# What else can we trim?
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(`Supplier Country`), gdp != "..") %>% # Not NAs
  View()
# Exercise 4. Now calculate the percentage:
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(`Supplier Country`), gdp != "..") %>%
  mutate(gdp = as.numeric(gdp),
         percentage = total/gdp * 100) %>%
  arrange(desc(percentage))

# Exercise 5. Turn it into a ggplot graph like before with ordered countries by % of gdp used for contract, only include the top 10
out <- joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(`Supplier Country`), gdp != "..") %>%
  mutate(gdp = as.numeric(gdp),
         percentage = total/gdp * 100) %>%
  arrange(desc(percentage))

out %>%
  head(10) %>%
  ggplot() +
  geom_bar(aes(y = percentage, x = reorder(`Country Name`, percentage)), # add ?reorder()
           stat = "identity", fill = "skyblue", position = "dodge") +
  theme_minimal() +
  # Clean up the axis and add content
  labs(x = NULL, y = NULL,
       title = "Top 10 Countries in Percent of GDP spent on Contract Awards",
       subtitle = "Fiscal year 2020",
       caption = "Data from World Bank") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  # tilt the y axis a little + change size
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(angle = 45, vjust = 0.5, size = 8))

# Bonus: Correlation GDP and Contracts
out %>%
  ungroup() %>%
  select(gdp, total) %>%
  cor()
  
out %>%
  ggplot() +
  geom_point(aes(x = gdp, y = total)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards")
  






