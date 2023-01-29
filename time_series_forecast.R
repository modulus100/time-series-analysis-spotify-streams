library(forecast)
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

# Global
streams <- unlist(country_time_series[39])
df <- data.frame(dates, streams)

# dates
spotify_ts <- ts(df$streams, start=c(2018), end = c(2020), frequency = 97)
arima_ts <- spotify_ts
# names(arima_ts) <- unlist(dates)

m1 <- auto.arima(spotify_ts, seasonal = TRUE)

summary(m1)
accuracy(forecast(m1))
checkresiduals(m1)
# start(df)

ddata <- decompose(spotify_ts, "multiplicative")
plot(ddata)

# plot(ddata$trend)
#
# plot(ddata$seasonal)
#
# plot(ddata$random)


# df.ts <- ts(df$streams, start=c(2018), end = c(2022), frequency = 54)
# print(df.ts)
#
# plot.ts(df.ts, col="blue",main="Airline Passengers", xlab = "Date", ylab="Number of Passengers")

