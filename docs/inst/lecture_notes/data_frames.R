# Day 2: Data Frames

# 1. Review --------------------------------------------------------------------
# Load data:
library(gapminder)

# Examine data.frame
class(gapminder)
head(gapminder, 3)

# View
?View
View(gapminder)

# Summarize
summary(gapminder)

# 2. Variables inside a data frame ---------------------------------------------
# Access them with `$`
gapminder$lifeExp

# We can create new data frames with variables from a data frame:
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
# What is a factor?
# Factors are the data objects which are used to categorize the data and store it as levels.
# They can store both strings and integers.
# They are useful in the columns which have a limited number of unique values.
table(gapminder$continent) # Count observations from each continent.
# Factors
table(factor(gapminder$continent, levels = c("Asia", "Africa", "Americas", "Europe", "Oceania")))
# Plot it
?barplot
barplot(table(gapminder$continent)) # Plot it

# 3. More plots!
# Load your data:
library(tidyverse)
# We explore ggplot2 in more detail later; for now, lets look at how we can use categorical data to tease out patterns in our data
p <- ggplot(data = gapminder[gapminder$continent != "Oceania", ],
            # Describe how variable in the data ar mapped to visual properties
            aes(x = gdpPercap, y = lifeExp)) 
# View it:
p
# Add a geom:
p + geom_point() # scatterplot
# Change the color of the points based on a variable:
p + geom_point(aes(color = continent))
# Split you graph into multiple panels based on a variable:
p + geom_point() + facet_wrap(vars(continent))
# Add a smoothed line to aid the eye on seeing patterns on your data:
p + geom_point() + facet_wrap(vars(continent)) + geom_smooth(lwd = 1,
                                                             se = FALSE)
