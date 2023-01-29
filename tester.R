library(tidyverse)
library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(lubridate)
library(dplyr)


Sale_of_fish <- read.csv("Fish dataset.csv",header=T)

head(Sale_of_fish) #See head of the dataset
tail(Sale_of_fish) #see tail of the dataset



adj <- select(Sale_of_fish, c("start", "sales"))
adj$sales <- as.numeric(gsub(' ', '', adj$sales))
adj$start <- dmy(adj$start)

adj$logr <- log(lag(adj$sales)) - log(adj$sales)
head(adj)
narm <- function (x) {
  x[is.na(x)] <- 1
  return(x)
}



max_Date <- max(adj$start)
min_Date <- min(adj$start)
test_ts <- ts(adj$logr, end=c(year(max_Date), month(max_Date)),start=c(year(min_Date), month(min_Date)),frequency=12)

logr <- adj
logr
plot(test_ts)
plot(stl(test_ts,s.window = "periodic"))

acf <- acf(test_ts, main='ACF Plot', lag.max=100)
pacf.logr <- pacf(test_ts, main='PACF Plot', lag.max=100)

#Augmented Dickey-Fuller(ADF) Test
print(adf.test(test_ts))


m1 <- auto.arima(test_ts, seasonal = TRUE)
summary(m1)
accuracy(forecast(m1))

checkresiduals(m1)