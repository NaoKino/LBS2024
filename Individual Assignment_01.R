# Individual Assignment_01 / Naoya Kinoshita

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

# Understanding Data

# 各年ごとのフライト数を集計
flight_counts <- flights %>%
  group_by(year) %>%
  summarise(total_flights = n())

# グラフの作成
ggplot(data = flight_counts, aes(x = factor(year), y = total_flights)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Flight Count", title = "Flight Count by Year") +
  theme_minimal()
          

# 月の英語の月名を定義
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 月ごとのOrigin別フライト数を集計
flight_counts <- flights %>%
  mutate(month = month) %>%
  group_by(month) %>%
  summarise(total_flights = n()) %>%
  mutate(month = factor(month, levels = 1:12, labels = month_names)) %>%
  arrange(month)

# 表の作成
flight_table <- flight_counts %>%
  spread(key = month, value = total_flights, fill = 0)

# 棒グラフの作成
ggplot(flight_counts, aes(x = month, y = total_flights)) +
  geom_bar(stat = "identity", fill = "transparent", color = "blue") +
  geom_text(aes(label = total_flights), vjust = -0.5, color = "blue") +
  labs(x = "Month", y = "Flight Count", title = "Flight Count by Month") +
  theme_minimal()


# Originごとのフライト数を集計
origin_counts <- flights %>%
  group_by(month, origin) %>%
  summarise(total_flights = n()) %>%
  spread(key = origin, value = total_flights, fill = 0)

# Destごとのフライト数を集計
dest_counts <- flights %>%
  group_by(month, dest) %>%
  summarise(total_flights = n()) %>%
  spread(key = dest, value = total_flights, fill = 0)

# Originごとの表を表示
print(origin_counts)

# Destごとの表を表示
print(dest_counts)

# Problem 01

# Q1: 到着遅延が2時間以上のフライト
q1_flights <- flights %>%
  filter(arr_delay > 120)

# Q2: ヒューストン行きで、United、American、Deltaによって運航されたフライト
q2_flights <- flights %>%
  filter(dest %in% c("IAH", "HOU"),
         carrier %in% c("UA", "AA", "DL"))

# Q3: 7月、8月、9月に出発したフライト
q3_flights <- flights %>%
  filter(month %in% c(7, 8, 9))

# Q4: 到着が2時間以上遅延したが、出発は遅延しなかったフライト
q4_flights <- flights %>%
  filter(arr_delay > 120,
         dep_delay <= 0)

# Q5: 出発が1時間以上遅延し、フライト中に30分以上遅延を取り戻したフライト
q5_flights <- flights %>%
  filter(dep_delay >= 60,
         (arr_delay - dep_delay) > 30)

# 各問に対するフライトの数を表示
answer <- data.frame(
  Q1 = nrow(q1_flights),
  Q2 = nrow(q2_flights),
  Q3 = nrow(q3_flights),
  Q4 = nrow(q4_flights),
  Q5 = nrow(q5_flights)
)

answer

# Problem 2

# キャンセルされたフライトの割合を計算
cancelled_proportion <- flights %>%
  group_by(month) %>%
  summarise(cancelled_proportion = sum(is.na(dep_delay) | is.na(arr_delay)) / n())

# 棒グラフを作成
ggplot(cancelled_proportion, aes(x = month, y = cancelled_proportion)) +
  geom_bar(stat = "identity", fill = "transparent", color = "blue") +
  geom_text(aes(label = scales::percent(cancelled_proportion)), vjust = -0.5, color = "blue") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Cancelled Flights Proportion") +
  theme_minimal()
