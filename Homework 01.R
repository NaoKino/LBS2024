library(nycflights13)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)


dplyr::glimpse(flights)

flights %>%
# count() is the same as group_by()... summarise()
# `sort=TRUE` gives table in descending order
count(origin, sort = TRUE) %>%

# mutate() generates a new column called `prop` which is the proportion of flights
# calculated as number of flights `n` divided by the `sum(n)`
mutate(prop = n/sum(n))

# Which origin airports had the greatest number of flights?
origin <- flights %>%
  group_by(origin) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights))
  kable(origin)

# What was the longest arr_delay?
# 表が汚いから修正したい
longest_arr_delay <- flights %>%
  filter(arr_delay == max(arr_delay, na.rm = TRUE)) %>%
  select(year, month, day, dep_time, sched_dep_time, dep_delay, arr_time, sched_arr_time, arr_delay, carrier, flight, tailnum, origin, dest, air_time, distance, hour, minute, time_hour)
  
kable(longest_arr_delay)

# What was the average arr_delay for a carrier, say UA?
average_arr_delay_UA <- flights %>%
  filter(carrier == "UA") %>%
  summarise(average_arr_delay = mean(arr_delay, na.rm = TRUE))

kable(average_arr_delay_UA)

# What was the average dep_delay and arr_delay for all carriers, ranked in descending order of number of flights operated by each carrier?
average_delay_by_carrier <- flights %>%
  group_by(carrier) %>%
  summarise(
    average_dep_delay = mean(dep_delay, na.rm = TRUE),
    average_arr_delay = mean(arr_delay, na.rm = TRUE),
    total_flights = n()
  ) %>%
  arrange(desc(total_flights))

kable(average_delay_by_carrier)
          