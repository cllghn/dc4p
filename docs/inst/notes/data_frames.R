# Day 2: Data Frames

# 1. Review --------------------------------------------------------------------

# Load data:
library(gapminder)
library(tidyverse)

# Examine data.frame
class(gapminder)
head(gapminder, 3)
str(gapminder)

# Summarize
summary(gapminder)

# 2. Variables inside a data frame ---------------------------------------------
# Access them with `$`
gapminder$lifeExp

# Numeric variables:
class(gapminder$lifeExp)
head(gapminder$lifeExp)
summary(gapminder$lifeExp)
hist(gapminder$lifeExp)

# Categorical variables:
class(gapminder$continent)
table(gapminder$continent) # Count observations from each contienent
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
