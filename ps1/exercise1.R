# Name of submitter: <Malik>
library(dplyr)
library(stringr)

# Exercise 1: Define the function sq that squares a single number x
sq <- function(x) {
  x^2  
}

# Exercise 2: From the `starwars` data, get all the non-human characters with yellow or blue-gray eyes.
# Keep all the columns.
non_human_eyes <- starwars # FIXME

starwars %>% filter(species != "Human" , grepl("yellow|blue-gray", eye_color))

# Exercise 3: write the body of the function `non_human_hair` that takes a single argument.
# This argument is a subset from the `starwars` data, and your function should return all the
# non-human characters who could possibly have brown, auburn, or no hair
# Keep only the following columns: name, species, eye_color, homeworld, and hair_color IN THAT ORDER
# Order the rows by species, then eye_color, both ascending alphabetically
non_human_hair <- function(df) {
  df %>% 
    filter(species != "Human", grepl("brown|auburn|none", hair_color)) %>% 
      select(name, species,eye_color, homeworld, hair_color)
}

non_human_hair(starwars)

#Use the `msleep` data (bulit-in dataset in the ggplot2 package) for Exercises 4-7

library(ggplot2)

#Exercise 4. Get all the animals who are heavier than the average bodyweight in the data
#Keep the name and bodywt of these animals
#Order the rows by bodyweight in a descending order
heavy_animals <- filter(msleep, bodywt > mean(bodywt))
heavy_animals <- select(heavy_animals[order(heavy_animals$bodywt), ], name, bodywt)

#Exercise 5. Create a new column called brainwt_ratio showing the ratio of
# of brain mass to the total body weight. Round the ratio to 4 digits. Keep the name and brainwt colums
# and keep the 10 animals with the highest relative brain mass.

msleep['brainwt_ratio'] = round(msleep['brainwt'] / msleep['bodywt'], 4)
clever_animals <- msleep[order(msleep$brainwt_ratio, decreasing=TRUE), ][0:10,][c('name', 'bodywt')]


#Exercise 6 Create a new column called brainwt_ratio, and keep only this column.
# Use the transmute command
brainweight <- 
  msleep %>% transmute(brainwt_ratio, brainwt / bodywt)

#Exercise 7 Check whether carnivores, herbivores, insectivores, or omnivores sleep more.
# First, remove the rows where vore is missing (NA)
# Create a data table with 4 rows and 3 columns showing the average,
# and the standard deviation for total sleep time for these 4 groups
meansleep_by_vore <- 
  drop_na(msleep) %>% 
  group_by(vore) %>% 
  summarize(
    avg_total_sleep = mean(sleep_total), 
    sd_total_sleep = sd(sleep_total)
    )






