library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyverse)
library(dtwclust)
library(dtwclust)
library(rlist)

# source("parser.R")

ds <- read_csv("charts.csv")

# normalizy genre, but works extremelly slow
# df <- df |>
#   modify_at("artist_genres", to_single_genre)

# take period
ds <- ds %>%
  filter(between(date, as.Date('2018-01-01'), as.Date('2021-12-30')))

# build times series for each country
grouped_by_country <- ds %>%
  select(streams, country, date, artist_genres) %>%
  arrange(date) %>%
  group_by(country, date) %>%
  summarise(streams = sum(streams)) %>%
  group_split(country)

country_time_series <- grouped_by_country %>%
  map(~.x %>% pluck("streams"))

countries <- grouped_by_country %>%
  map(~.x %>% pluck("country")) %>%
  map(\(countries) {
    countries[1][1]
  })

names(country_time_series) <- unlist(countries)

# make times seties the same length
max_list_len <- max(lengths(country_time_series))

country_time_series <- country_time_series %>%
  map(\(time_series) {
    len <- length(time_series)

    if (len < max_list_len) {
      to_add <- max_list_len - len
      zeros <- rep(0, to_add)
      return(append(zeros, time_series))
    }

    time_series
  })


# times series clustering
unique_countries <- ds %>%
  select(country) %>%
  distinct

k <- as.integer(nrow(unique_countries) / 4)
k <- 20L

pc <- tsclust(country_time_series, type = "partitional", k = 20L,
              distance = "dtw_basic", centroid = "pam",
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))

hc <- tsclust(country_time_series, type = "hierarchical", k = 20L,
              distance = "sbd", trace = TRUE,
              control = hierarchical_control(method = "average"))

print(pc)
print(hc)

plot(pc)
plot(hc)