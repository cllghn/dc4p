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
#           - names(events_2020) == names(events2021) | ncol(events_2020) == ncol(events_2021)
#       When column binding, ensure the row are aligned. This method is not recommended!
#           - nrow(events_2020) == nrow(events_2021), why? dim(events_2020) or dim(events_2021)
#   - Joins: Combining data using a designated variable as a key. That is we join one table with another if they match on the key.
#           - See syllabus tidy verbs animated
  
# Binds ------------------------------------------------------------------------
# First let's proceed to load two data sets
mca_2000 <- read_csv(file = "data/2000_Major_Contract_Awards.csv") %>%
  janitor::clean_names()
mca_2001 <- read_csv(file = "data/2001_Major_Contract_Awards.csv") %>%
  janitor::clean_names()

# Inspect them:
dim(mca_2000)
dim(mca_2001)
names(mca_2000)
names(mca_2001)
# Are the names the same?
names(mca_2000) == names(mca_2001)

# Then row bind them
?rbind
mca_2000_2001 <- rbind(mca_2000, mca_2001)
# Inspect them:
dim(mca_2000_2001)
# Inspect the data, how? By year.
mca_2000_2001 %>%
  # First group
  group_by(fiscal_year) %>%
  # Then count
  summarize(observations = n()) # alternative table(mca_2000_2001$fiscal_year)


# Exercise 1. Now your turn.
# Read in all the remaining years (2002 to 2021), bind them into a data.frame named mca, we will be using this for the rest of the class today
#   - Keep in mind, you will have to bind all object into an object "mca"
#   - Only read the CSV files labeled *_Major_Contract_Awards.csv...
# You could do it this way:
# mca <- rbind(
#   read_csv(file = "data/2000_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(),
#   read_csv(file = "data/2001_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(), 
#   read_csv(file = "data/2002_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(),
#   read_csv(file = "data/2003_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(),
#   read_csv(file = "data/2004_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(),
#   read_csv(file = "data/2005_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(),
#   read_csv(file = "data/2006_Major_Contract_Awards.csv") %>%
#     janitor::clean_names(),
#   ...
# )
# Massive cheat:
# names <- list.files(path = "data/",
#                     pattern = "*_Major_Contract_Awards.csv$",
#                     full.names = TRUE)
# mca <- purrr::map_df(names, ~.x %>% read_csv() %>% janitor::clean_names())

# Inspect the data....
dim(mca)
mca %>%
  # First group
  group_by(fiscal_year) %>%
  # Then count
  summarize(observations = n())


# Joins ------------------------------------------------------------------------
# See syllabus tidy verbs animated
# We will join the mca data to a second table and run some analysis.

# Let's load a new data set:
(codelist <- read_csv(file = "data/codelist.csv"))
# Look at countrycode documentation on the syllabus.

# Inspect the data....
dim(codelist) 
names(codelist)
head(codelist)

# Question: How could we join mca with codelist? What key could we use?

# Exercise 2.  Since the codelist data is rather extensive, let's reduce it down to some relevant variables.
#   - You will create a new object "sub_codelist"
#   - It will contain only the following variables: the key (iso2c), continent, iso.name.en, region, and iso3

# sub_codelist <- codelist %>% # now you complete this statement

# Answer:
sub_codelist <- codelist %>%
    select(iso2c, iso3c, iso.name.en, continent, region)
# Take a look
sub_codelist #Notice the variables not in MCA: continent and region


# Goal, join data. Work our way to a breakdown of investment by continent and then region.
# left_join() ==================================================================
# left_join includes all records from the left side and matched rows from the right table
# ?left_join
# Start with mca data
mca %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c"))
# Did it join? Read tibble dimensions at the top

# Reorganize variables to test that:
mca %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  # First select the variables you want on the left, then everything else:
  select(supplier_country, supplier_country_code, iso.name.en,
         everything()) # Selects all variables

# Did the merging process work? 
# One way to test this looking for NAs, which usually indicate an unsuccessful join
mca %>%
  # Join
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  # Select
  select(supplier_country, supplier_country_code, iso.name.en,
         everything()) %>%
  # Filter out variables that have a character in the iso.name.en variable
  filter(is.na(iso.name.en)) %>% # Pause here
  # Now group to create a summary
  group_by(supplier_country, supplier_country_code, iso.name.en) %>%
  # Summarize
  summarize(n = n()) 

# What happens if we remove the filter
mca %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  select(supplier_country, supplier_country_code, iso.name.en,
         everything()) %>%
  # filter(is.na(iso.name.en)) %>%
  group_by(supplier_country, supplier_country_code, iso.name.en) %>%
  summarize(n = n()) %>% # Pause
  # Now view
  View()

# Recode and remove!
# Remove
# Kosovo = Does not have ISO
# MeOthersico remove
# Remove St Marteen
# Remove World
mca %>%
  # Filter out noise
  filter(!supplier_country %in% c("MeOthersico", "World", "St Maarten", "Kosovo"))

# Recode
# Central Africa = CF
# Congo, Democrat = CR
# Namibia = "NA"
# Netherlands = NL
# Serbia = RS
# Timor-Leste = TL
# West Bank and Gaza = PS
# Yemen = YE
mca %>%
  # Filter out noise
  filter(!supplier_country %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(supplier_country_code = case_when(supplier_country == "Central Africa" ~ "CF",
                                           supplier_country == "Congo, Democrat" ~ "CR",
                                           supplier_country == "Namibia" ~ "NA",
                                           supplier_country == "Netherlands Ant" ~ "NL",
                                           supplier_country == "Serbia" ~ "RS",
                                           supplier_country == "Timor-Leste" ~ "TL",
                                           supplier_country == "West Bank and G" ~ "PS",
                                           supplier_country == "Yemen, Republic" ~ "YE",
                                           TRUE ~ supplier_country_code))

# Put it all together
mca %>%
  # Filter out noise
  filter(!supplier_country %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(supplier_country_code = case_when(supplier_country == "Central Africa" ~ "CF",
                                             supplier_country == "Congo, Democrat" ~ "CR",
                                             supplier_country == "Namibia" ~ "NA",
                                             supplier_country == "Netherlands Ant" ~ "NL",
                                             supplier_country == "Serbia" ~ "RS",
                                             supplier_country == "Timor-Leste" ~ "TL",
                                             supplier_country == "West Bank and G" ~ "PS",
                                             supplier_country == "Yemen, Republic" ~ "YE",
                                             TRUE ~supplier_country_code
                                             )) %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  select(supplier_country, supplier_country_code, iso.name.en,
        everything()) %>%
  filter(is.na(iso.name.en)) %>%
  group_by(supplier_country, supplier_country_code, iso.name.en) %>%
  summarize(n = n()) 

# Error in the codelist read
sub_codelist <- sub_codelist %>%
  mutate(iso2c = if_else(iso.name.en == "Namibia", "NA", iso2c))

#Try again, will this process work?
mca %>%
  # Filter out noise
  filter(!supplier_country %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(supplier_country_code = case_when(supplier_country == "Central Africa" ~ "CF",
                                             supplier_country == "Congo, Democrat" ~ "CR",
                                             supplier_country == "Namibia" ~ "NA",
                                             supplier_country == "Netherlands Ant" ~ "NL",
                                             supplier_country == "Serbia" ~ "RS",
                                             supplier_country == "Timor-Leste" ~ "TL",
                                             supplier_country == "West Bank and G" ~ "PS",
                                             supplier_country == "Yemen, Republic" ~ "YE",
                                             TRUE ~supplier_country_code
  )) %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  select(supplier_country, supplier_country_code, iso.name.en,
         everything()) %>%
  filter(is.na(iso.name.en)) %>%
  group_by(supplier_country, supplier_country_code, iso.name.en) %>%
  summarize(n = n()) 
# All done!

# Create a new object:
(joined <- mca %>%
  # Filter out noise
  filter(!supplier_country %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(supplier_country_code = case_when(supplier_country == "Central Africa" ~ "CF",
                                             supplier_country == "Congo, Democrat" ~ "CR",
                                             supplier_country == "Namibia" ~ "NA",
                                             supplier_country == "Netherlands Ant" ~ "NL",
                                             supplier_country == "Serbia" ~ "RS",
                                             supplier_country == "Timor-Leste" ~ "TL",
                                             supplier_country == "West Bank and G" ~ "PS",
                                             supplier_country == "Yemen, Republic" ~ "YE",
                                             TRUE ~supplier_country_code
  )) %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  select(supplier_country, supplier_country_code, iso.name.en,
         everything()))


# Exercise 3. With this new data set, calculate the total in contract awarded by continent in 2020
#   - You must filter for year 2020
#   - Group by continent
#   - Sum the total amount of contracts
joined %>%
  # First filter
  filter(fiscal_year == 2020) %>% 
  # Now group by the variable
  group_by(continent) %>%
  # Create a new variable
  summarise(total_2020 = sum(total_contract_amount_usd))

# We could graph this
# https://www.rstudio.com/resources/cheatsheets/ #discrete x and continuous y
joined %>%
  # First filter
  filter(fiscal_year == 2020) %>% 
  # Now group by the variable
  group_by(continent) %>%
  # Create a new variable
  summarise(total_2020 = sum(total_contract_amount_usd)) %>%
  # First ggplot it
  ggplot() + # Note the differnt pipe
  # Next pass on a geom
  geom_col(aes(x = continent, y = total_2020))

# Let's assign it to a graph object:
joined %>%
  # First filter
  filter(fiscal_year == 2020) %>% 
  # Now group by the variable
  group_by(continent) %>%
  # Create a new variable
  summarise(total_2020 = sum(total_contract_amount_usd)) %>%
  # First ggplot it
  ggplot()  + 
  geom_col(aes(x = continent, y = total_2020), fill = "darkgreen") +
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

# Bonus: Filter by to 2020, group by region and supplier, calculate total amount.
total_by_continent_country <- joined %>%
    filter(fiscal_year == 2020) %>%
    group_by(continent, supplier_country, supplier_country_code) %>%
    summarize(total = sum(total_contract_amount_usd))
# Graph it! Discrete x and continuous y

# Layer each layer...
ggplot() +
  geom_bar(data = filter(total_by_continent_country, continent == "Asia"), 
           # ?reorder
           aes(y = total, x = reorder(supplier_country, -total)),
           stat = "identity", fill = "skyblue", position = "dodge") +
  # Themes
  theme_minimal() +
  # Labels and Legends: Clean up the axis and add content
  labs(x = NULL, y = NULL,
       title = "Total Contract Amount by Country for the Asian Continent in 2020",
       subtitle = "Amounts in USD",
       caption = "Data from World Bank") +
  # General purpose scale correction, ?scales::comma
  scale_y_continuous(labels = scales::comma) +
  # Themes: tilt the y axis a little + change size
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
        axis.text.y = element_text(angle = 45, vjust = 0.5, size = 8))

# right_join() =================================================================
# Back to joins: https://r4ds.had.co.nz/relational-data.html#understanding-joins
# right_join includes all records from the right side and matched rows from the right left
# Simulation: Say we want to calculate the percent of GDP awarded in contracts for 2020
# To do that, we are going to have to get the GDP for 2020 for all countries
# Look at WB gdp in current us on syllabus
# First read new data:
gdp <- read_csv(file = "data/wb_gdp_2000to2020.csv")
# Inspect it:
head(gdp) 

# Issues:
#   - Wide data -> pivot_longer()
#   - Missing GDP in characters and they include ".."

# tidy the data:
(gdp <- gdp %>%
  # First pivot
  select(-`Series Name`, -`Series Code`) %>%
  pivot_longer(!c("Country Code", "Country Name"), names_to = "year", values_to = "gdp") %>%
  # Now mutate: https://github.com/rstudio/cheatsheets/blob/master/strings.pdf
  mutate(year = str_replace(year, "[:space:].*+", ""), # regex is space and everything after
         year = as.numeric(year),
         gdp  = ifelse(gdp == "..", NA, gdp))) # Missing values

# I need the contract values in mca and the gdp in the gdp dataset.
# Q. What keys do we have between mca and gdp? None? 
# A. We need to join the sub_contrycode data set to use the iso3 keys, then merge gdp

# Example of joining as before
(joined <- mca %>%
  # Filter out noise
  filter(!supplier_country %in% c("MeOthersico", "World", "St Maarten", "Kosovo")) %>%
  # Recode
  mutate(supplier_country_code = case_when(supplier_country == "Central Africa" ~ "CF",
                                             supplier_country == "Congo, Democrat" ~ "CR",
                                             supplier_country == "Namibia" ~ "NA",
                                             supplier_country == "Netherlands Ant" ~ "NL",
                                             supplier_country == "Serbia" ~ "RS",
                                             supplier_country == "Timor-Leste" ~ "TL",
                                             supplier_country == "West Bank and G" ~ "PS",
                                             supplier_country == "Yemen, Republic" ~ "YE",
                                             TRUE ~supplier_country_code)) %>%
  left_join(sub_codelist, by = c("supplier_country_code" = "iso2c")) %>%
  select(supplier_country, supplier_country_code, iso3c, iso.name.en,
         everything()))

# For this example, let's only focus on the 2020 numbers per country.
# In order to join joined and gdp, joined must be cleaned up:
#   - Filter out data to only inlcude 2020
#   - group country and iso3c codes
#   - Finally summarize to create a total variable which is a sum of total contract ammouns per country
(joined <- joined %>%
  filter(fiscal_year == 2020) %>%
  group_by(supplier_country, iso3c) %>%
  summarise(total_2020 = sum(total_contract_amount_usd)))

# Now join them:
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code"))
# Take a look
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  View()
# How many countries in the World Bank GDP data not in contracts?

# Trim them:
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(supplier_country)) %>% # Not NAs
  View()
# What else can we trim?

# gdp = ".."
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(supplier_country), gdp != "..") %>% # No ..
  View()

# Exercise 4. Now calculate the percentage, what should you add to the pipeline to calculate the percentage of gdp?
# Answer
joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(supplier_country), gdp != "..") %>%
  mutate(gdp = as.numeric(gdp),
         percentage = total_2020/gdp * 100) %>%
  arrange(desc(percentage))

# Exercise 5. Turn it into a ggplot graph like before with ordered countries by % of gdp used for contract, only include the top 10
out <- joined %>%
  right_join(filter(gdp, year == 2020), by = c("iso3c" = "Country Code")) %>%
  # First clean
  filter(!is.na(supplier_country), gdp != "..") %>%
  mutate(gdp = as.numeric(gdp),
         percentage = total_2020/gdp * 100) %>%
  arrange(desc(percentage))

out %>%
  head(10) %>%
  ggplot() +
  geom_bar(aes(y = percentage, x = reorder(`Country Name`, -percentage)), # add ?reorder()
           stat = "identity", fill = "skyblue", position = "dodge") +
  theme_minimal() +
  # Clean up the axis and add content
  labs(x = NULL, y = NULL,
       title = "Top 10 Countries in Percent of GDP spent on Contract Awards",
       subtitle = "Fiscal year 2020",
       caption = "Data from World Bank") +
  scale_y_continuous(labels = scales::percent) + # ? scales::percent
  # coord_flip() +
  # tilt the y axis a little + change size
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(angle = 45, vjust = 0.5, size = 8))

# Bonus: Correlation GDP and Contracts
# How associated is one variable with another 
?cor()
# inspect your output
out # May need ungrouping

out %>%
  ungroup()

out %>%
  ungroup() %>%
  # Select the variables you want
  select(gdp, total_2020) %>% 
  # See if they are correlated
  cor()

# x and y are continuous
out %>%
  ggplot() +
  geom_point(aes(x = gdp, y = total_2020)) + # remember Rob's lesson
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards")

out %>%
  ggplot() +
  geom_point(aes(x = log(gdp), y = log(total_2020))) + # remember Rob's lesson
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards")
# What relationship do we see?
?geom_smooth
out %>%
  ggplot() +
  geom_point(aes(x = log(gdp), y = log(total_2020))) + # remember Rob's lesson
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards") +
  # fit a smooth curve through points in a scatter plot to aid the eye in seeing patterns
  geom_smooth(aes(x = log(gdp), y = log(total_2020)), se = FALSE)

# Temporal visualization -------------------------------------------------------
# Follow up on the morning using time and dates with ggplot and lubridate
# https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# To do this, we will switch gears to GDP per capita
(gdppc <- read_csv(file = "data/wb_gdppc_2000to2020.csv") %>%
  janitor::clean_names())
# Like before, we need to clean it up to make it tidy data:
(gdppc <- read_csv(file = "data/wb_gdppc_2000to2020.csv") %>%
  janitor::clean_names() %>%
  # First pivot
  select(-series_name, -series_code) %>%
  pivot_longer(!c("country_code", "country_name"), names_to = "year", values_to = "gdp"))
# clean up the year and gdp  
(gdppc <- read_csv(file = "data/wb_gdppc_2000to2020.csv") %>%
  janitor::clean_names() %>%
  # First pivot
  select(-series_name, -series_code) %>%
  pivot_longer(!c("country_code", "country_name"), names_to = "year",
               values_to = "gdp") %>%
  # Mutate
  mutate(year = str_replace(year, ".*[yr]", ""),
         year = as.numeric(year),
         gdp  = ifelse(gdp == "..", NA, gdp),
         gdp  = as.numeric(gdp)))

# Let's join gdp and sub_codelist to create some interesting visualizations
gdppc %>%
  left_join(sub_codelist, by = c("country_code" = "iso3c"))

# Did it work?
gdppc %>%
  left_join(sub_codelist, by = c("country_code" = "iso3c")) %>%
  filter(is.na(iso.name.en)) %>%
  # Did it work?
  View()

# Assign it to a new object
gdppc <- gdppc %>%
  left_join(sub_codelist, by = c("country_code" = "iso3c")) %>%
  filter(!is.na(iso.name.en))

# Let's make a time series visualization of GDP, by country
gdppc <- gdppc  %>%
  #filter(year >= 2010) %>%
  mutate(year = stringr::str_c("12-",  year),
         # Set at date
         year = lubridate::my(year),
         # Advance one month, then subtract a day
         year = lubridate::ceiling_date(year, "month") - lubridate::days(1))

# Two continious functions... see cheatsheet... use lines
?geom_line
# ggplot This may take a while
ggplot(data = gdppc) +
  geom_line(aes(x = year, y = gdp, group = country_name), color = "lightgrey") +
  geom_line(
    data = filter(gdppc, country_name == "Mongolia"),
    aes(x = year, y = gdp, group = country_name), color = "black")

# How did Mongolia compare to average gdppc in each region?
# How to calculate the gdppc in each region
gdppc %>% filter(!is.na(gdp)) %>% group_by(region, year) %>% summarise(avg_gdppc = mean(gdp))

# We can first layer Mongolia. what is the x axis and y axis?
ggplot() +
  # First add Mongolia
  geom_line(
    data = filter(gdppc, country_name == "Mongolia"),
    aes(x = year, y = gdp, group = country_name), color = "black")

# Now layer the gdppc in each region
ggplot() +
  # First add Mongolia
  geom_line(
    data = filter(gdppc, country_name == "Mongolia"),
    aes(x = year, y = gdp, group = country_name), color = "black") +
  # Now add the regions
  geom_line(
    data = gdppc %>% filter(!is.na(gdp)) %>% group_by(region, year) %>% summarise(avg_gdppc = mean(gdp)),
    # The aes matches the data...
    aes(x = year, y = avg_gdppc, group = region, color = region))

# Dates allow us to stack different datasets on the same axis!
# Let's clean it up
ggplot() +
  # Now add the regions
  geom_line(
    data = gdppc %>% filter(!is.na(gdp)) %>% group_by(region, year) %>% summarise(avg_gdppc = mean(gdp)),
    # The aes matches the data...
    aes(x = year, y = avg_gdppc, group = region, color = region)) +
  # Change the order
  geom_line(
    data = filter(gdppc, country_name == "Mongolia"),
    aes(x = year, y = gdp, group = country_name), color = "black", 
    linetype = "dotted") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  labs(x = NULL, y = "GDP Per Capita", colour = "Region", 
       title = "GDP Percapita Accross Regions of the World Compared to Mongolia",
       subtitle = "Mongolia represented a black dotted line",
       caption = "Data from World Bank")  

# Exercise 6. Modify the prior graph to reflect GDPPC per continent...
ggplot() +
  # Now add the regions
  geom_line(
    data = gdppc %>% filter(!is.na(gdp)) %>% group_by(continent, year) %>% summarise(avg_gdppc = mean(gdp)),
    # The aes matches the data...
    aes(x = year, y = avg_gdppc, group = continent, color = continent)) +
  # Change the order
  geom_line(
    data = filter(gdppc, country_name == "Mongolia"),
    aes(x = year, y = gdp, group = country_name), color = "black", 
    linetype = "dotted") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  labs(x = NULL, y = "GDP Per Capita", colour = "Continent", 
       title = "GDP Percapita Accross Continents of the World Compared to Mongolia",
       subtitle = "Mongolia represented a black dotted line",
       caption = "Data from World Bank")  

# Change color: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
ggplot() +
  # Now add the regions
  geom_line(
    data = gdppc %>% filter(!is.na(gdp)) %>% group_by(continent, year) %>% summarise(avg_gdppc = mean(gdp)),
    # The aes matches the data...
    aes(x = year, y = avg_gdppc, group = continent, color = continent),
    # Line weight
    lwd = 1) +
  # Change the order
  geom_line(
    data = filter(gdppc, country_name == "Mongolia"),
    aes(x = year, y = gdp, group = country_name), color = "black", 
    linetype = "dotted", lwd = 1) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  labs(x = NULL, y = "GDP Per Capita", colour = "Continent", 
       title = "GDP Percapita Accross Continents of the World Compared to Mongolia",
       subtitle = "Mongolia represented a black dotted line",
       caption = "Data from World Bank") 

  
