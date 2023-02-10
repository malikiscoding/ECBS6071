install.packages("nycflights13")
install.packages("tidyverse")
library(nycflights13)
library(tidyverse)
dim(flights)
filter(flights, month == 1)
unique(flights$month)
jan_flights <- filter(flights, month == 1)
unique(jan_flights[['month']])
dim(jan_flights)

stopifnot(unique(jan_flights[['flights']]) == c(4,2,3,4))
filter(flights, month == 1, day == 1)

(feb1 <- filter(flights, month == 2, day == 1))
near(2, 10, tol=20)

view(flights)

dep_ok_arr_not <- filter(flights, dep_delay <= 120 & arr_delay >= 120)
dep_ok_arr_not == filter(flights, dep_delay <= 120 , arr_delay >= 120)

NA^0
O^NA  
FALSE^0
