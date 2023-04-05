# load required packages
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(dplyr)
library(leaflet.extras)

# import data
df <- read_csv("data/Chicago_Crime_Rate.csv")

# drop unused columns
df <- select(df, -'CASE#', -BLOCK, -IUCR, -BEAT, -WARD, -`FBI CD`, -`X COORDINATE`, -`Y COORDINATE`, -LOCATION)

# create columns (month, year, day, weekday, hour)
df <- df |> 
  mutate(date_time = mdy_hms(`DATE  OF OCCURRENCE`),
         month = month(date_time, label = TRUE),
         year = year(date_time),
         day = day(date_time),
         weekday = wday(date_time, label = TRUE),
         hour = hour(date_time)) |> 
  select(-`DATE  OF OCCURRENCE`, -date_time)

# Count the number of NAs in each column
na_count <- df |> is.na() |> colSums()

# Drop rows with any NAs
df <- df |> na.omit()

# Identify duplicate rows
dup_rows <- df[duplicated(df),]

# Display the first 10 duplicate rows
head(dup_rows, n = 10)

# Remove the duplicate rows
df <- unique(df)

df |>
  count(`PRIMARY DESCRIPTION`, sort = TRUE) |>
  head(n = 10) |>
  ggplot(aes(x = fct_reorder(`PRIMARY DESCRIPTION`, n), y = n, fill = `PRIMARY DESCRIPTION`)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Count", x = NULL) +
  ggtitle("Top 10 Most Frequent Crimes in Chcicago") +
  guides(fill = "none")

# Exporting the plot as an image to the "output" folder with a width of 8 inches and a height of 6 inches
ggsave("output/Top10MostFrequentCrimes.png", dpi = 300, width = 8, height = 6)

# Filter rows where the "ARREST" column has a value of "Y"
df |> filter(ARREST == "Y") |>
  # Count the number of occurrences of each unique value of "PRIMARY DESCRIPTION"
  count(`PRIMARY DESCRIPTION`) |>
  # Sort the resulting counts in descending order
  arrange(desc(n)) |>
  # Reorder the "PRIMARY DESCRIPTION" column according to the sorted counts
  mutate(`PRIMARY DESCRIPTION` = fct_reorder(`PRIMARY DESCRIPTION`, n)) |>
  # Create a bar plot using "PRIMARY DESCRIPTION" as the x-axis variable, and the count as the y-axis variable
  ggplot(aes(x = `PRIMARY DESCRIPTION`, y = n, fill = `PRIMARY DESCRIPTION`)) +
  # Create a bar chart with each bar representing the count of a unique "PRIMARY DESCRIPTION" value
  geom_bar(stat = "identity") +
  # Rotate the x-axis labels by 90 degrees for better readability
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Set the y-axis label to "Count"
  labs(y = "Count", x = NULL) +
  # Set the plot title to "Frequency of Crimes Resulting in Arrests in Chicago"
  ggtitle("Frequency of Crimes Resulting in Arrests in Chicago") +
  # Remove the legend
  guides(fill = "none")

# Exporting the plot as an image to the "output" folder with a width of 10 inches and a height of 8 inches
ggsave("output/CrimesResultingInArrests.png", dpi = 300, width = 10, height = 8)

## When do thefts occur?

ggplot(df |> filter(`PRIMARY DESCRIPTION` == "THEFT"), aes(x = hour)) +
  geom_bar(fill = "black") +
  labs(x = "Hour", y = "Count") +
  ggtitle("Frequency of Thefts by Hour of the Day") +
  theme(axis.text.x = element_text(size = 30), axis.text.y = element_text(size = 30), axis.title = element_text(size = 40))

# Exporting the plot as an image to the "output" folder with a width of 12 inches and a height of 10 inches
ggsave("output/TheftsByHour.png", dpi = 300, width = 12, height = 10)

# There are spikes at midday and midnight. Midday spike could be reasoned by people commuting for lunch and being more exposed to thiefs. 

ggplot(df |> filter(`PRIMARY DESCRIPTION` == "THEFT"), aes(x = weekday, fill = weekday)) +
  geom_bar() +
  labs(x = "", y = "Count") +
  ggtitle("Frequency of Thefts by Day of the Week") +
  theme(axis.text.x = element_text(size = 30), axis.text.y = element_text(size = 30), axis.title = element_text(size = 40))

#Thefts peak on friday, perhaps because there are more people moving around to new places like bars, clubs, parks, etc.

# Exporting the plot as an image to the "output" folder with a width of 12 inches and a height of 10 inches
ggsave("output/TheftsByDay.png", dpi = 300, width = 12, height = 10)

# Thefts by hour of the day
ggplot(df |> filter(`PRIMARY DESCRIPTION` == "BATTERY"), aes(x = hour)) +
  geom_bar(color = "black", fill = "black") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(x = "Hour", y = "Count") +
  ggtitle("Batteries by Hour of the Day")

# Exporting the plot as an image to the "output" folder with a width of 12 inches and a height of 10 inches
ggsave("output/BatteriesByHour.png", dpi = 300, width = 12, height = 10)

# Drop NAs and duplicates
df_filtered <- df |>
  drop_na(LATITUDE, LONGITUDE) |>
  distinct(LATITUDE, LONGITUDE, .keep_all = TRUE)

# Create leaflet map
crime_map <- leaflet(df_filtered) |>
  setView(lng = -87.7098, lat = 41.8836, zoom = 11) |>
  addTiles() # Add tile layer with roads

# Add heatmap
crime_map <- crime_map |> addHeatmap(
  data = df_filtered, 
  lng = ~ LONGITUDE, 
  lat = ~ LATITUDE, 
  radius = 10
)

# Display map
crime_map
