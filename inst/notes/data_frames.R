# Day 2: Data Frames

# 1. Review --------------------------------------------------------------------

# Load data:
library(gapminder)
library(tidyverse)

# Examine data.frame
class(gapminder)
head(gapminder, 3)
str(gapminder)

# View
?View
View(gapminder)

# Summarize
summary(gapminder)

# 2. Variables inside a data frame ---------------------------------------------
# Access them with `$`
gapminder$lifeExp

# We can create new dataframes with variables from a dataframe:
?data.frame
my_df <- data.frame("my_country" = gapminder$country,
           "my_gdppc"   = gapminder$gdpPercap)
# Add a column:
my_df$my_lifeexp <- gapminder$lifeExp
head(my_df)

# Reminder, because we have different data type we can do multiple things:
# Numeric variables:
class(gapminder$lifeExp)
head(gapminder$lifeExp)
summary(gapminder$lifeExp)
?hist
hist(gapminder$lifeExp)

# Categorical variables:
class(gapminder$continent)
# Factors are the data objects which are used to categorize the data and store it as levels.
# They can store both strings and integers.
# They are useful in the columns which have a limited number of unique values.
table(gapminder$continent) # Count observations from each contienent
# Factors
table(factor(gapminder$continent, levels = c("Asia", "Africa", "Americas", "Europe", "Oceania")))
# Plot it
?barplot
barplot(table(gapminder$continent)) # Plot it

# 3. More plots!
# Getting more bang for your buck fusing data types in graphs:
# We explore ggplot2 in more detail later; for now, lets look at how we can use categorical data to tease out patterns in our data
p <- ggplot(gapminder[gapminder$continent != "Oceania", ],
            aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() # scatterplot
p
p + geom_point(aes(color = continent)) # map continent to color
p + geom_point() + facet_wrap(~ continent) # facets
p + geom_point() + facet_wrap(~ continent) + geom_smooth(lwd = 1, se = FALSE)
