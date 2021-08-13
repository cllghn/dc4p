# Day 2: Relational Data
# First begin by installing new tools:
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("sf")

# Now load data:
library(tidyverse)
library(rnaturalearth)
library(sf)

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
names <- list.files(path = "data/",
                    pattern = "*_Major_Contract_Awards.csv$",
                    full.names = TRUE)
mca <- purrr::map_df(names, ~.x %>% read_csv() %>% janitor::clean_names())
Major_Contract_Awards_2000_2005 <- mca[mca$fiscal_year < 2006, ]
Major_Contract_Awards_2006_2010 <- mca[mca$fiscal_year >= 2006 & mca$fiscal_year < 2011, ]
Major_Contract_Awards_2011_2015 <- mca[mca$fiscal_year >= 2011 & mca$fiscal_year < 2015, ]
Major_Contract_Awards_2016_2021 <- mca[mca$fiscal_year >= 2016 & mca$fiscal_year < 2022, ]
write_csv(Major_Contract_Awards_2000_2005, file = "data/Major_Contract_Awards_2000_2005.csv")
write_csv(Major_Contract_Awards_2006_2010, file = "data/Major_Contract_Awards_2006_2010.csv")
write_csv(Major_Contract_Awards_2011_2015, file = "data/Major_Contract_Awards_2011_2015.csv")
write_csv(Major_Contract_Awards_2016_2021, file = "data/Major_Contract_Awards_2016_2021.csv")


# Inspect the data....
dim(mca)
mca %>%
  # First group
  group_by(fiscal_year) %>%
  # Then count
  summarize(observations = n())

# Look at investment by country in 2020:
mca %>%
  # Filter
  filter(fiscal_year == "2020") %>%
  # Group
  group_by(fiscal_year) %>%
  # Calculate
  summarize(investment = scales::dollar(sum(total_contract_amount_usd)))
  
# Goal today: Get a breakdown of investment by country in Asia.
# Problem: mca data does not include information on the continent for each supplier country...
# Solution: JOIN!

# Joins ------------------------------------------------------------------------
# See syllabus tidy verbs animated
# We will join the mca data to a second table and run some analysis.
# First load some data:
spatial_data <- rnaturalearth::ne_countries(type = "countries", returnclass = "sf")
# Inspect it
head(spatial_data)
dim(spatial_data)
names(spatial_data)
# What is it?
class(spatial_data)
# Look closer:
spatial_data %>%
  select(name, iso_a3, iso_a2, wb_a3, wb_a2, continent, region_un)
# Look up documentation: https://r-spatial.github.io/sf/articles/sf1.html
# What is the geometry?
sf::st_geometry(spatial_data) 
# Look closer
sf::st_geometry(spatial_data)[[1]]
# Geometry allows us to plot it
spatial_data %>%
  select(name, wb_a3, wb_a2, continent, region_un) %>%
  plot()

# st_crs(spatial_data)
# Because it is a data.frame we can use dplyr funtions
spatial_data %>%
  select(name, wb_a3, wb_a2, continent, region_un) %>%
  filter(continent == "Asia") %>%
  plot()

# To join, we need keys...
# Question: How could we join mca with codelist? What key could we use?
head(mca)
head(spatial_data)

# To join:
# left_join() ==================================================================
# left_join includes all records from the left side and matched rows from the right table
# ?left_join
# Start with mca data
mca %>%
  # Select
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2"))
# Did it join? Read tibble dimensions at the top

# Continue by selecting columns to narrow variables and then turn to Asia:
mca %>%
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
  # First select the variables you want on the left, then everything else:
  select(supplier_country, supplier_country_code, continent, total_contract_amount_usd,
         major_sector, fiscal_year, region_un, geometry) %>%
  # Now filter by country
  filter(continent == "Asia") 

# Now summarize
mca %>%
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
  # First select the variables you want on the left, then everything else:
  select(supplier_country, supplier_country_code, continent, total_contract_amount_usd,
         major_sector, fiscal_year, region_un, geometry) %>%
  # Now filter by country
  filter(continent == "Asia") %>%
  group_by(supplier_country, fiscal_year) %>%
  summarise(total = sum(total_contract_amount_usd))
# All done!

# Filter it by year!
mca %>%
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
  # First select the variables you want on the left, then everything else:
  select(supplier_country, supplier_country_code, continent, total_contract_amount_usd,
         major_sector, fiscal_year, region_un, geometry) %>%
  # Now filter by country
  filter(continent == "Asia", fiscal_year = 2020) %>%
  group_by(supplier_country, fiscal_year) %>%
  summarise(total = sum(total_contract_amount_usd)) 

# Can we put this on a map? Yes
mca %>%
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
  class() # Not spatial

mca %>%
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
  st_as_sf() %>%
  # First select the variables you want on the left, then everything else:
  select(supplier_country, supplier_country_code, continent, total_contract_amount_usd,
         major_sector, fiscal_year, region_un, wb_a3, geometry) %>%
  # Now filter by country
  filter(continent == "Asia", fiscal_year == 2020) %>%
  group_by(supplier_country, supplier_country_code, fiscal_year) %>%
  summarise(total = sum(total_contract_amount_usd)) 

# Assign it:
totals_2020 <- mca %>%
  select(fiscal_year, supplier_country, supplier_country_code,
         total_contract_amount_usd, major_sector) %>%
  left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
  st_as_sf() %>%
  # First select the variables you want on the left, then everything else:
  select(supplier_country, supplier_country_code, continent, total_contract_amount_usd,
         major_sector, fiscal_year, region_un, wb_a3, geometry) %>%
  # Now filter by country
  filter(continent == "Asia", fiscal_year == 2020) %>%
  group_by(supplier_country, wb_a3, fiscal_year) %>%
  summarise(total = sum(total_contract_amount_usd)) 

ggplot() + # step 1
  geom_sf(data = totals_2020) # step 2

ggplot() + # step 1
  geom_sf(data = totals_2020, aes(fill = total)) + # step 3
  scale_fill_continuous(high = "blue",
                        low = "lightblue",  # Step 4
                        label = scales::dollar) + # Step 5
  # Fix axis
  theme(axis.text.x = element_blank(), # Step 6
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # Remove grey background
        panel.grid.major = element_blank(), # Step 7
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  # Fix legend name
  labs(fill = "Total Contract Amount",
       title = "World Bank Total Contract Amount by Country",
       subtitle = "2020 Subset for Asian Contient",
       caption = "Institute for Strategic Studies") # Step 8
# Assign last: 
p <- ggplot() + 
  geom_sf(data = totals_2020, aes(fill = total)) +
  scale_fill_continuous(high = "blue",
                        low = "lightblue",  
                        label = scales::dollar) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(fill = "Total Contract Amount",
       title = "World Bank Total Contract Amount by Country",
       subtitle = "2020 Subset for Asian Contient",
       caption = "Institute for Strategic Studies")
p

# Challenge: Modify the code to see which sectors did each country invest in 2020:
# mca %>%
#   select(fiscal_year, supplier_country, supplier_country_code,
#          total_contract_amount_usd, major_sector) %>%
#   left_join(spatial_data, by = c("supplier_country_code" = "wb_a2")) %>%
#   # First select the variables you want on the left, then everything else:
#   select(supplier_country, supplier_country_code, continent, total_contract_amount_usd,
#          major_sector, fiscal_year, region_un, geometry) %>%
#   # Now filter by country
#   filter(continent == "Asia") %>%
#   group_by(supplier_country, fiscal_year, major_sector) %>% # Just add variable name
#   summarise(total = sum(total_contract_amount_usd)) 


# right_join() =================================================================
# Back to joins: https://r4ds.had.co.nz/relational-data.html#understanding-joins
# right_join includes all records from the right side and matched rows from the right left
# Simulation: Say we want to calculate the percent of GDP awarded in contracts for 2020
# To do that, we are going to have to get the GDP for 2020 for all countries
# Look at WB gdp in current us on syllabus
# First read new data:
gdp <- read_csv(file = "data/wb_gdp_2000to2020.csv") %>%
  janitor::clean_names()
# Inspect it:
head(gdp) 

# Issues:
#   - Wide data -> pivot_longer()
#   - Missing GDP in characters and they include ".."

# tidy the data:
 gdp %>%
  # First pivot
  select(-series_name, -series_code) %>%
  pivot_longer(!c("country_code", "country_name"), names_to = "year", values_to = "gdp") %>%
  # Now mutate: https://github.com/rstudio/cheatsheets/blob/master/strings.pdf
  mutate(year = str_replace(year, ".*(yr)", ""), # regex is space and everything after
         year = as.numeric(year),
         gdp  = ifelse(gdp == "..", NA, gdp)) # Missing values

# Join the totals_2020 and the gdp data
# First since the totals_2020 data is for year 2020 filter out the gdp
gdp_2020 <- gdp %>%
   # First pivot
   select(-series_name, -series_code) %>%
   pivot_longer(!c("country_code", "country_name"), names_to = "year", values_to = "gdp") %>%
   # Now mutate: https://github.com/rstudio/cheatsheets/blob/master/strings.pdf
   mutate(year = str_replace(year, ".*(yr)", ""), # regex is space and everything after
          year = as.numeric(year),
          gdp  = ifelse(gdp == "..", NA, gdp)) %>%
   filter(year == 2020)

# Join
gdp_2020 %>%
  right_join(totals_2020, by = c("country_code" = "wb_a3"))
# Did it work? Number of totals? Add View()?
# Is it a spatial object?
gdp_2020 %>%
  right_join(totals_2020, by = c("country_code" = "wb_a3")) %>%
  class()
# Make is spatial
gdp_2020 %>%
  right_join(totals_2020, by = c("country_code" = "wb_a3")) %>%
  st_as_sf()

# Exercise 4. Now calculate the percentage of GDP spend on WB awards.
# What should you add to the pipeline to calculate the percentage of gdp?
# Answer
gdp_2020 %>%
  right_join(totals_2020, by = c("country_code" = "wb_a3")) %>%
  st_as_sf() %>%
  mutate(gdp = as.numeric(gdp),
         percentage = total/gdp * 100) %>%
  select(country_name, percentage, everything()) %>%
  arrange(desc(percentage))

# Now write over the object
gdp_2020 <- gdp_2020 %>%
  right_join(totals_2020, by = c("country_code" = "wb_a3")) %>%
  st_as_sf() %>%
  mutate(gdp = as.numeric(gdp),
         percentage = total/gdp * 100) %>%
  select(country_name, percentage, everything()) %>%
  arrange(desc(percentage))

# ggplot: bar chart
ggplot(data = gdp_2020) +
  geom_bar(aes(y = percentage, x = reorder(country_name, -percentage)), # add ?reorder()
           stat = "identity", fill = "skyblue", position = "dodge") +
  theme_minimal() +
  # Clean up the axis and add content
  labs(x = NULL, y = NULL,
       title = "Percent of GDP spent on WB Contract Awards in Asian Continent",
       subtitle = "Fiscal year 2020",
       caption = "Data from World Bank") +
  scale_y_continuous(labels = scales::percent) + # ? scales::percent
  # coord_flip() +
  # tilt the y axis a little + change size
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(angle = 45, vjust = 0.5, size = 8))

# Scatter plot: x and y are continuous
ggplot(data = gdp_2020) +
  geom_point(aes(x = gdp, y = total)) + # remember Rob's lesson
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards")

ggplot(data = gdp_2020) +
  geom_point(aes(x = log(gdp), y = log(total))) + # remember Rob's lesson
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards")

# What relationship do we see?
?geom_smooth
ggplot(data = gdp_2020) +
  geom_point(aes(x = log(gdp), y = log(total))) + # remember Rob's lesson
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(x = "GDP", y = "2020 Total Contract Awards") +
  # fit a smooth curve through points in a scatter plot to aid the eye in seeing patterns
  geom_smooth(aes(x = log(gdp), y = log(total)), se = FALSE)

# Map of percent of GDP spent on WB awards
ggplot() + 
  geom_sf(data = gdp_2020, aes(fill = percentage)) +
  scale_fill_continuous(high = "blue",
                        low = "lightblue",  
                        label = scales::percent) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(fill = "Percent of GDP spent on WB Contracts",
       title = "Percent of GDP Spent on World Bank Contracts",
       subtitle = "2020 Subset for Asian Contient",
       caption = "Institute for Strategic Studies")
