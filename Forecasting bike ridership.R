# Forecasting daily bicycle ridership
# Data set downloaded from Udemy program


# import packages
library(tidyverse)
library(tidyquant)
library(forecast)
library(tseries)
library(data.table)
library(corrplot)

# load the data set
df1 <- fread("//Users//raviswanath//Downloads//Case\ Studies//Bike-Sharing-Dataset//day.csv")

# create a date column agnostic of time
df1$date <- as.Date(df1$dteday, format='%Y-%m-%d')

needed_cols_train <- c("season", "weathersit", "temp", "windspeed", "cnt", "date")
needed_cols_test <- c("season", "weathersit", "temp", "windspeed", "date")

# subset dataframe
data <- df1[, needed_cols_train, with=FALSE]

# create 7 day moving averages of riders, temperature and humidity to reduce the variance
data$ma_temp <- ma(data$temp, order=7)
data$ma_windspeed <- ma(data$windspeed, order=7)
data$ma_riders <- ma(data$cnt, order=7)

# convert moving averages back to int from ts
data$ma_temp <- as.numeric(data$ma_temp)
data$ma_windspeed <- as.numeric(data$ma_windspeed)
data$ma_riders <- as.numeric(data$ma_riders)

# transforming the riders using a box-cox transformation
data$ma_riders <- BoxCox(data$ma_riders, lambda="auto")

# define a cut off date for training data and convert riders into a time series object
cut_off_date <- c("2012-07-24")
data$riders <- ts(data$ma_riders)

# splitting into training and test sets
train_data <- data[date <= cut_off_date]
test_data <- data[date > cut_off_date]

riders <- ts(train_data$riders)

# using auto.arima to get a first basic forecast
fit1 <- auto.arima(riders, seasonal=TRUE,
                   xreg=train_data[, .(ma_temp, ma_windspeed, season, weathersit)])

checkresiduals(fit1)
# the check residuals funtion above helps with proving that
# there is significant information left in the residuals which could be 
# incorportaed into the model. 

# The PCF plot shows that the lags at orders 5 and 7 are most significant
# Changing the MA degrees to 7
fit2 <- Arima(riders, order=c(5,1,7),
              xreg=train_data[, .(ma_temp, ma_windspeed, season, weathersit)])

checkresiduals(fit2)

#  the p-value is still at 0.01, indicating that the residuals
# are still not white noise. However, we cannot see any significant 
# spikes on the PACF plot. 

# The distribution of the lags is slightly right skewed, we should be careful 
# about trusting the forecast method generated CI numbers 

# cross validation 
# get the xreg data seperated out for easing out tsCV
xreg_train <- train_data[,.(ma_temp, ma_windspeed, season, weathersit)]
xreg_test <- test_data[,.(ma_temp, ma_windspeed, season, weathersit)]
xregs <- rbind(xreg_train, xreg_test)

# function returning forecast object to use inside tsCV
farma <- function(y, xreg, h){
  # since CV is dynamically rolling, xreg needs to behave the same way
  X <- xreg[1:length(y), ]
  # cannot forecast if length(xreg) < length(y)+ h
  if(nrow(xreg) < length(y) + h){
    stop("Not enough xreg data for forecasting")
  }
  # xreg for rolling forecast
  new_X <- xreg[length(y) + (1:h), ]
  
  # using the tuned order
  fit <- Arima(y, xreg=X, order=c(5,1,7))
  
  forecast(fit, xreg=new_X, h=h)
}

# Cross validating the model using tsCV
e <- tsCV(riders, farma, xreg=xregs, h=7)
# using h=7 since we are using a smoothened series, rolled at order=7

# Calculating the MSE and RMSE for Cross validated error
mse <- colMeans(e^2, na.rm=T)
rmse <- colMeans(e^2, na.rm=T) %>% sqrt()

# making the forecast using (5,1,7) order model
output1 <- forecast(fit2, 
                    xreg=xreg_test)

# Forecast accuracy, MAPE and RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

MAPE = function(act, x){
  mean(abs(act-x)/act)*100
}

# getting the point forecast
x <- output1[[4]]
x <- as.vector(x)

actual_riders_test <- ts(test_data$ma_riders)
actual_riders_test <- as.vector(actual_riders_test)

RMSE(actual_riders_test, x)
MAPE(actual_riders_test, x)


# plot of the CV errors and final forecast
data.frame(h=1:7, RMSE= rmse) %>% ggplot(aes(x=h, y=RMSE)) + geom_point()
data.frame(h=1:7, MSE= mse) %>% ggplot(aes(x=h, y=MSE)) + geom_point()
autoplot(output1)
###########################################################################################


