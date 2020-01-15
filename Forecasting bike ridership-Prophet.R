# Forecasting daily bi-cycle ridership in Seattle
# Data set downloaded from Kaggle
# Forecast using prophet

# import packages
library(tidyverse)
library(prophet)
library(forecast)
library(tseries)
library(data.table)

# load the data set
data <- fread("//Users//raviswanath//Downloads//Case\ Studies//Bike-Sharing-Dataset//day.csv")

# change the data type of date
data$date <- as.Date(data$dteday, format="%Y-%m-%d")

# create 7 day moving averages of riders, temperature and humidity to reduce the variance
data$ma_temp <- ma(ts(data$temp), order=7)
data$ma_windspeed <- ma(ts(data$windspeed), order=7)
data$ma_riders <- ma(ts(data$cnt), order=7)

# convert the type to the moving avgs to numeric 
data$ma_temp <- as.numeric(data$ma_temp)
data$ma_windspeed <- as.numeric(data$ma_windspeed)
data$ma_riders <- as.numeric(data$ma_riders)

# remove na's generated at the beginning and end
data <- data[!(is.na(ma_riders))]

# transforming the riders using a box-cox transformation
data$riders <- BoxCox(data$ma_riders, lambda="auto")

# store the lambda value to re-trnasform and change
# the type to numeric
lambda <- BoxCox.lambda(data$ma_riders)
data$riders <- as.numeric(data$riders)

# initialize a cut off date to seperate
# training and test data sets
cut_off_date <- c("2012-07-24")

# split the training and test data frames
train_data <- data[date <= cut_off_date]
test_data <- data[date > cut_off_date]

# intialize prophet object and add regressors
m <- prophet()
m <- add_regressor(m, 'windspeed')
m <- add_regressor(m,'temp')
m <- add_regressor(m,'season')
m <- add_regressor(m, 'weathersit')

# filter the train and test data for needed columns only
train <- train_data[, .(ds=date, y=riders, temp=ma_temp, 
                        windspeed=ma_windspeed, season, weathersit)]
test <- test_data[, .(ds=date, y=riders, temp=ma_temp, 
                      windspeed=ma_windspeed, season, weathersit)]
# fit the model
m <- fit.prophet(m, train)
future <- make_future_dataframe(m, periods=157)

# update regressors in the future data frame (include train and test)
future$windspeed <- data$ma_windspeed
future$temp <- data$ma_temp
future$season <- data$season
future$weathersit <- data$weathersit

# make forecasts and plot
forecast1 <- predict(m, future)
plot(m,forecast1) + xlab("Time") + ylab("Ridership") + ggtitle("Forecasting using Prophet: Forecast with CI")+
  theme(plot.title = element_text(hjust = 0.5, size=18))
# components of the forecast
prophet_plot_components(m, forecast1)

# cross validate the model
cv <- cross_validation(m, period=30, horizon=60, units='days')
performance <- performance_metrics(cv)

# to compare with the ARIMA model's performance
performance %>% filter(horizon==7)
