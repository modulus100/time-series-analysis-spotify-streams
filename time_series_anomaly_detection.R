library(anomalize)
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyverse)
library(dtwclust)
library(dtwclust)
library(rlist)

ds <- read_csv("charts.csv")

ds$date <- as.Date(ds$date , format = "%y-%m-%d")

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

dates <- ds %>%
  select(date) %>%
  arrange(date)

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

dates <- unique(dates)
# dates <- unique(dates) %>% as_tibble()

countries <- unlist(countries)


# Andorra time series
# time_series <- unlist(country_time_series[1])
# df <- data.frame(time_series, dates)

# Japan time series
# time_series <- unlist(country_time_series[39])
# df <- data.frame(time_series, dates)

# Estonian time series
# time_series <- unlist(country_time_series[21])
# df <- data.frame(time_series, dates)

# Russian time series
# time_series <- unlist(country_time_series[59])
# df <- data.frame(time_series, dates)

# Korean time series
# time_series <- unlist(country_time_series[40])
# df <- data.frame(time_series, dates)

# Global time series
# time_series <- unlist(country_time_series[27])
# df <- data.frame(time_series, dates)

# test
time_series <- unlist(country_time_series[39])
df <- data.frame(time_series, dates)


# anomaly detection
df %>%
  as_tibble() %>%
  time_decompose(time_series) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 1 Year")

# anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
# time_decompose(time_series, method = "stl", frequency = "auto", trend = "auto") %>%