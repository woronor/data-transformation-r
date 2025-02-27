# install packages
install.packages( c("dplyr", "tidyr", "nycflights23", "ggplot2") )

# load library
library(nycflights23)
library(dplyr)
library(tidyr)
library(ggplot2)

# previews data
View(flights)
glimpse(flights)

# check missing values: NA
is.na(flights)

# 5 query with 5 questions
# [1] high carrier
high_carrier <- flights %>%
  group_by(carrier) |>
  summarise(n_flights = n()) |>
  left_join(airlines, by = "carrier") |>
  select(name, n_flights) |>
  arrange(desc(n_flights))

high_carrier

# [2] Top 10 frequent destination
top_dest <- flights %>%
  group_by(dest) %>%
  summarize(n_flights_dest = n()) %>%
  arrange(desc(n_flights_dest)) %>%
  head(10)

top_dest

# [3] avg_delay per carrier
delayed_carr <- flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  left_join(airlines, by = "carrier") %>%
  select(name, avg_delay) %>%
  arrange(desc(avg_delay))

delayed_carr

# [4] flights dep_delayed
dep_delayed <- flights %>%
  mutate(dep_hour = dep_time %/% 100) %>%
  filter((dep_hour >= 7 & dep_hour <= 9) | (dep_hour >= 17 & dep_hour <= 19)) %>%
  group_by(origin) %>%
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  select(origin, name, avg_dep_delay) %>%
  rename("code" = "origin")

dep_delayed

# [5] all_flights_month
all_flights_month <- flights %>%
  group_by(month) %>%
  summarize(total_flights = n()) %>%
  arrange(desc(total_flights)) %>%
  ggplot(aes(x = factor(month), y = total_flights)) +
  geom_col(fill = "burlywood1") +
  labs(title = "Total Flights each Month",
       x = "Month",
       y = "Total Flights") +
  scale_x_discrete(labels = month.abb)

all_flights_month
