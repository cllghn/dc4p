# Load library:
library(tidyverse)
 
# If you haven't loaded the data from the prior module, load it:
file_path <- "data/un_comtrade_mongolia_commodities_exchanges_2018to2020.csv"
un_comtrade <- read_csv(file = file_path) %>%
  janitor::clean_names()
# Otherwise just clean it and reshape it:
(un_visdata <- un_comtrade %>%  select(rt_title, rg_desc, trade_value) %>%
  mutate(clean_desc = case_when(rg_desc == "Re-Export" ~ "Export",
                                rg_desc == "Re-Import" ~ "Import",
                                TRUE ~ rg_desc # Keeps all present values the same
  )) %>%
  group_by(rt_title, clean_desc) %>%
  summarise(total = sum(trade_value)) %>%
  pivot_wider(names_from = clean_desc, values_from = total))


# 1. Introduce ggplot2 ---------------------------------------------------------
# Format for ggplot2 is:
# <DATA> %>%
#   ggplot(aes(<MAPPINGS>)) +
#   <GEOM_FUNCTION>()

# 2. Prep data/basic vis  ------------------------------------------------------
# Our data is our data.frame
head(un_visdata)

# Describe that this data has two continuous variables
#hist(un_visdata$Export)
#hist(un_visdata$Import)
#max(un_visdata$Export)
# Clean int up
un_visdata <- un_visdata %>%
  mutate(log_export = log(Export),
         log_import = log(Import),
         log_export = if_else(is.na(log_export),0,log_export),
         log_import = if_else(is.na(log_import),0,log_import),
         Export = if_else(is.na(Export),0,Export),
         Import = if_else(is.na(Import),0,Import),
         balance = Export - Import)

un_visdata


# 3. Scatterplot ---------------------------------------------------------------
# Scatterplot of Exports and Imports
# NOTE: Emphasize the use of + instead of pipes
un_visdata %>%
  #filter(!is.na(Export) & !is.na(Import)) %>% # Removing comment fixes error
  ggplot(aes(x = Export, y = Import)) +
  geom_point()

# Same as:
# ggplot(data = un_visdata, aes(x = Export, y = Import)) +
#   geom_point()

un_visdata %>%
  ggplot(aes(x = log_export, y = log_import)) +
  geom_point()

# Create a graph and stack layers:
first_vis <- un_visdata %>%
  ggplot(aes(x = log_export, y = log_import))

first_vis
# Add layer
first_vis +
  geom_point()


# Set transparency
?geom_point

un_visdata %>%
  ggplot(aes(x = log_export, y = log_import)) +
  geom_point(alpha = 0.5)

# Alternative option
?geom_jitter

un_visdata %>%
  ggplot(aes(x = log_export, y = log_import)) +
  geom_jitter()

# Mix them:
un_visdata %>%
  ggplot(aes(x = log_export, y = log_import)) +
  geom_jitter(alpha = 0.5,
              width = 0.2,
              height = 0.2)

# Add single color color
un_visdata %>%
  ggplot(aes(x = log_export, y = log_import)) +
  geom_jitter(alpha = 0.5,
              color = "blue", # Let them pick colors
              width = 0.2,
              height = 0.2)

# Color by variable
# Coloring by Balance: First create a new category based on prior data:
un_visdata %>%
  mutate(TradeBalance = if_else(balance > 0, "Deficit", "Surplus" ))

un_visdata <- un_visdata %>%
  mutate(TradeBalance = ifelse(balance > 0, "Deficit", "Surplus"))

# aes() function indicates we're getting something from the data.frame
un_visdata %>%
  ggplot(aes(x = log_export, y = log_import)) +
  geom_jitter(aes(color = TradeBalance),
              alpha = 0.5,
              width = 0.2,
              height = 0.2)


# 4. Boxplot -------------------------------------------------------------------
?geom_boxplot
# Raw scores:
un_visdata %>%
  ggplot(aes(x = TradeBalance, y = Export)) +
  geom_boxplot()
# With logs:
un_visdata %>%
  ggplot(aes(x = TradeBalance, y = log_export)) +
  geom_boxplot()
# Remove fill
un_visdata %>%
  ggplot(aes(x = TradeBalance, y = log_export)) +
  geom_boxplot(alpha = 0)

un_visdata %>%
  ggplot(aes(x = TradeBalance, y = log_export)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5,
              color = "tomato",
              width = 0.2,
              height = 0.2)

# 4. Barchart ------------------------------------------------------------------
# Let's bring in a data set with more categorical variables:
major_contracts <- read_csv("data/Major_Contract_Awards.csv") %>%
  janitor::clean_names()
  
major_contracts_p7_plus <- major_contracts %>%
  filter(is.element(supplier_country_code, 
                    c("US", "GB", "FR", "CA", "JP", "IT", "DE", "RU", "CN")))
# OR:
# major_contracts_p7_plus <- major_contracts %>%
#   filter(supplier_country_code %in% c("US", "GB", "FR", "CA", "JP", "IT", "DE",
#                                       "RU", "CN"))

(contracts_vis2 <- major_contracts %>%
  group_by(region, supplier_country_code) %>%
  summarize(total_projects = n()))


# 4. Barchart ------------------------------------------------------------------
unique(major_contracts_p7_plus$region)

### Basic bar chart by region ###
# EAP East asia and Pacific
# ECA Europe and Central Asia
# AFW Western Africa
# AFR Africa
# LCR Latin American/Caribbean
# MNA Middle East
# Oth Other
# SAR South America

major_contracts_p7_plus %>%
  ggplot(aes(x = supplier_country_code)) +
  geom_bar()

major_contracts_p7_plus %>%
  ggplot(aes(x = region)) +
  geom_bar()


### Fill
major_contracts_p7_plus %>%
  ggplot(aes(x = region)) +
  geom_bar(fill = "tomato")

## Fill by country
major_contracts_p7_plus %>%
  ggplot(aes(x = region)) +
  geom_bar(aes(fill = supplier_country_code))

## Kind of hard to read
major_contracts_p7_plus %>%
  ggplot(aes(x = region)) +
  geom_bar(aes(fill = supplier_country_code), position = "dodge")

## Instead of number of projects, we can look at sum of USD
(money_from <- major_contracts_p7_plus %>%
  group_by(region, supplier_country_code) %>%
  summarize(total_usd = sum(total_contract_amount_usd)))

?geom_bar
?stat_count

money_from %>%
  ggplot(aes(x = region, y = total_usd, fill = supplier_country_code)) +
  geom_bar(stat = "identity", position = "dodge")

# 5. Adding Labels and Facets  -------------------------------------------------
?labs

money_from %>%
  ggplot(aes(x = region, y = total_usd, fill = supplier_country_code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "World Bank Projects",
       fill = "Supplier Country",
       x = "Region",
       y = "$ for Projects")

# This can still be hard to read 
# with all of them next to each other
?facet_wrap

money_from %>%
  ggplot(aes(y = total_usd, x = supplier_country_code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "World Bank Projects",
       x = "Supplier Country",
       y = "$ for Projects") +
  facet_wrap(~region)

money_from %>%
  ggplot(aes(y = total_usd, x = supplier_country_code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "World Bank Projects",
       x = "Supplier Country",
       y = "$ for Projects") +
  facet_wrap(~region, scales = "free")

# Extra code:
# options(scipen=999)
# scale_x_continuous(labels = scales::comma)


### Divide by 1,000,000

# money_from <- money_from %>%
#   mutate(mils_usd = total_usd/1000000)

money_from %>%
  # Mutate prior to starting graph if you only need that variable for graph:
  mutate(mils_usd = total_usd/1000000) %>%
  # Begin graph:
  ggplot(aes(y = mils_usd, x = supplier_country_code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "World Bank Projects",
       x = "Supplier Country",
       y = "Millions of Dollars") +
  facet_wrap(~region, scales = "free") +
  # Remove scientific notation from the y scale
  scale_y_continuous(labels = scales::comma)