# Day 1: Programming Basics

# Identify the default panes:
#   - Console
#   - Environment/History
#   - Files/Plots/Packages/Help
# Note: The order of these can be modified!

# Keyboard shortcuts: Help > Keyboard Shortcut Help
# Save: Ctrl+S (Window) | Cmd+S (Mac)

# 1. Console input/outputs -----------------------------------------------------
1 # Simple input
1 + 1 # Inputs with arithmetic operator

# 2. Assignment: objectName <- value -------------------------------------------
# Insert assignment operator: Alt- (Window) | Option- (Mac)
x <- 3 * 4 # I hear "x gets 12"
x
# Note: Object names cannot start with a digit or certain characters.

# Cover logical Operators (google them "R logical operators"): https://www.statmethods.net/management/operators.html

# Save: Ctrl+S (Window) | Cmd+S (Mac)

# Do: Adopt conventions for demarcating words:
snake_case <- "snake_case convention"
use.periods <- "period.use convention"
camelCase <- "camelCase convention"

# Inspect your object:
snakeCase
use_periods
camel_Case

# 3. Functions: function_name(arg1 = val1, arg2 = val2, ...) { body } ----------
sum(1, 1, 1)
?sum # Lookup documentation
added_value <- sum(1, 2, 3)
added_value
# Don't forget up arrow
(second_addition <- sum(added_value, added_value)) # "print to screen"

# Not all functions take arguments:
date()
ls()
rm(second_addition)
rm(list = ls()) # Remove everything

# Open a script-----------------------------------------------------------------
# Tools > Global Options > Display > Show Margin > at 80
# Make sure your code is readable and compact

#Start Scripting
# The script editor is a great place to put code that you care about.

# You can make your own:
say_hello <- function() {print("hello world!")}
say_hello() # parenthesis call the function
# Look at the inside of the function
say_hello

# Build bigger functios:
check_packages <- function(.dependencies) {
  for (i in seq_along(.dependencies)) {
    if (!requireNamespace(.dependencies[[i]], quietly = TRUE)) {
      install.packages(.dependencies[[i]], dependencies = TRUE)
    }
  }
}
# What is going on?
# A. check_packages expects and argument named .dependencies
# B. Generate a sequence up to the length of the vector
# C. Evaluate each item in a vector to determine whether it can be loaded
# D. If it cannot be loaded, quietly install the missing package
packages <- c("tidyverse")
check_packages(.dependencies = packages)

# You should never include install.packages() or setwd() in a script that you share. 
# It’s very antisocial to change settings on someone else’s computer!

# RStudio diagnostics ----------------------------------------------------------
# The script editor will highlight syntax errors with a red squiggly line and a cross in the sidebar:
x y <- 10 # This will also throw an error
# Hoover over the error to find out what the problem is.
# RStudio will also let you know potential problems:
3 == NA # Hoover over

# 1. Go to the RStudio Tips twitter account, https://twitter.com/rstudiotips and find one tip that looks interesting.
# 2. What other common mistakes will RStudio diagnostics report? Read https://support.rstudio.com/hc/en-us/articles/205753617-Code-Diagnostics to find out.
#   Google: Code Diagnostics in the RStudio IDE

# 4. Words of Advise -----------------------------------------------------------
# A. Capitalization.
Sys.time() 
# vs.
sys.time()

# B. Spacing.
x<-1
x < -1

# C. Document, document, document.
Sys.time() #Return the system's current time and date

# D. Work smarter, not harder.
?Sys.time

# E. Ask, Google, look around... 
# Stackoverflow
# RStudio 

# 5. RStudio Project -----------------------------------------------------------
getwd() # Working directory:  Where R will look for I/O

# Create a Project: File > New Project... 
getwd() # Check the "home" is the Project

# I/O example from Stat545
a <- 2
b <- -3
sig_sq <- 0.5
x <- runif(40)
y <- a + b * x + rnorm(40, sd = sqrt(sig_sq))
(avg_x <- mean(x))
write(avg_x, "avg_x.txt") # Write out average
plot(x, y)
abline(a, b, col = "purple")
dev.print(pdf, "toy_line_plot.pdf") # Write out plot

# 6. Data Types and Data Structures --------------------------------------------

# Vectors: Atomic (homogeneous) or Lists (Heterogeneous)
# One-dimensional structures, think of a line of values on the same axis

# Atomic Vectors: Birds of a feather...
vector_lgl   <- c(TRUE, FALSE, TRUE)
vector_dbl    <- c(0x1, 2.0 , 3e0) # Floating point numbers
vector_int   <- c(1L, 2L, 1:3L) # Non fractional
vector_chr <- c("Hello", "world", "!") # Anything between " or '

# Interrogate a vector's type
?str
str(vector_lgl)
?class
class(vector_lgl)
typeof(vector_lgl)

# Vector functions
# Lenghts
length(vector_dbl)
# Sort
sort(vector_dbl)
# See counts of values
table(vector_lgl)
# See unique
unique(vector_lgl)
unique(vector_int)

# Selecting Vector Elements
# By position
vector_lgl[1]
vector_lgl[2]
# By range
vector_lgl[1:2]
# By value
vector_int[vector_int > 2]
vector_chr[vector_chr %in% c("Hello", "!")]

# Review logical Operators (google them "R logical operators"): https://www.statmethods.net/management/operators.html

# Math
log(vector_dbl)
max(vector_dbl)
min(vector_dbl)
cor(vector_dbl, vector_dbl)
sum(vector_dbl)
mean(vector_dbl)
media(vector_dbl)
quantile(vector_dbl)

# data.frames: Two-dimensional data structures
(my_df <- data.frame(vector_lgl,
                    vector_dbl,
                    vector_chr))

# Access columns
my_df$vector_lgl
my_df[ , 1] # or my_df[ ,"vector_dbl"]
my_df[2 ,]

# Examine type of data structure:
class(my_df)

# Discover dimensions:
nrow(my_df)
ncol(my_df)

# 7. Examine data in base R ----------------------------------------------------
# They may need to install this:
# install.packages("gapminder")
library(gapminder)

# Examine data.frame
class(gapminder)
head(gapminder, 3)
tail(gapminder, 3)
str(gapminder)
# Pull a vector
gapminder$country
# Cross-classify values
?table
table(gapminder$country)

# More ways to query data:
names(gapminder)
nrow(gapminder)
ncol(gapminder)
length(gapminder)

# Filter data
gapminder[1, ] # First row
gapminder[, 1] # First column
gapminder[gapminder$country == "Mongolia", ] # Rows matching a parameter
gapminder[gapminder$country == "Mongolia" & gapminder$year == 1992, ] # Multiple
# Perhaps you want to use a varialbe in a query
my_country <- "Mongolia"
gapminder[gapminder$country == my_country & gapminder$year == 1992, ] 
# Same with numbers 
highest_year <- max(gapminder_unfiltered$year)
gapminder[gapminder$country == my_country & gapminder$year == highest_year, ] 

# Statistical overview:
summary(gapminder)
summary(gapminder$lifeExp)
summary(gapminder$country)

model <- lm(lifeExp ~ gdpPercap, data = gapminder)
summary(model)

# Early visualization:
plot(lifeExp ~ year, gapminder)
plot(lifeExp ~ gdpPercap, gapminder)
plot(lifeExp ~ log(gdpPercap), gapminder)

# Exercise: Answer the following:
# What type of vector is the variable 'year'?
class(gapminder$year)
#   - How many observations are in the dataset for "Mongolia"?
table(gapminder$country) # or
gapminder[gapminder$country == "China", ]
#   - Which country has the highest GDP per capita?
max_gdppc <- max(gapminder$gdpPercap)
gapminder[gapminder$gdpPercap == max_gdppc, ] #or
gapminder[gapminder$gdpPercap ==  max(gapminder$gdpPercap), ]
