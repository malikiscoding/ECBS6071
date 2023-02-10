c(0,1)
c(0,0,0,0,0)
# 2. Everyting is an object
maliks_new_object <- "I am a fancy object" #comment? 
maliks_new_combined_object <- c(maliks_new_object, "Some apples, because they are healthy")
maliks_new_combined_object
help(c)
typeof(maliks_new_combined_object)
help(typeof)
list_of_p_languages <- c(
  "Racket",
  "SQL",
  "Python",
  "Javascript",
  "Matlab",
  "Maple",
  "Processing",
  "Assembly",
  "C",
  "C++",
  "C#",
  "Scratch",
  "DAX"
)

length(list_of_p_languages)

languages_heard_of <- c(
  10,
  12,
  8,
  11,
  11,
  10,
  10,
  7,
  10,
  10,
  9,
  8,
  11,
  9,
  9,
  10,
  10,
  14
)
length(languages_heard_of)

# 3. You do things using functions
mean(languages_heard_of)
mean(c(0,10))

# 4. Functions come in packages
library(ggplot2)
ggplot()
library(dplyr)
x <- c(0, 3.0, 2.9)
str(x)
x[0]
x[[1]]

# R wants every element of array to be of the same type.
# char is in priority

# help

help("summary")
summary(languages_heard_of)

## How ggplot works
library(tidyverse)
install.packages("gapminder")
library(gapminder)
gapminder
??gapminder
p <- ggplot(
  data = gapminder,
  mapping = aes(x = gdpPercap, y = lifeExp)
            )
p + geom_point()

# or in one go 
#p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp) + geom_point()

p + geom_smooth()





