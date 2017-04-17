library(xts)
library(TTR)
library(MLmetrics)
library(forecast)

# Changing work directory, reading training and testing data
setwd("D:/Rita/ppp-2017/ppp-2017/tasks/task1")

data <- read.csv(file = "training.csv", header = TRUE)
train <- xts(as.matrix(data[,2]), as.Date(strptime(data[,1], "%Y-%m-%d")))
ts_train <- ts(as.matrix(data[,2]), frequency=12, start=c(1959,1))
data <- read.csv(file = "testing.csv", header = TRUE)
test <- xts(as.matrix(data[,2]), as.Date(strptime(data[,1], "%Y-%m-%d")))
ts_test <- ts(as.matrix(data[,2]), frequency=12, start=c(1989,1))

# Plot of time series
plot.xts(train)

# Moving average
z1 <- SMA(train, 5)
lines(z1, col="red")

# Standard deviation
lines(train - z1 + 40, col="blue")

# Trend, seasonal, random, observed
de <- decompose(ts_train, type = "additive")
plot(de)
de <- decompose(ts_train, type = "multiplicative")
plot(de)

# Order of integration
int <- ndiffs(train)

# The first difference
di <- diff(train, differences = 1)

# Auto- and cross- covariance and -correlation 
acf(di, main = 'ACF for Differenced Series') # q = 4
pacf(di, main='PACF for Differenced Series') # p = 2

# Three different models
model_1 <- arima(ts_train, order = c(2,1,0))

fr_1 <- forecast.Arima(model_1, h = 60)
r_1 <- R2_Score(y_true = ts_test, y_pred = fr_1$mean)

model_2 <- arima(ts_train, order = c(0,1,4))

fr_2 <- forecast.Arima(model_2, h = 60)
r_2 <- R2_Score(y_true = ts_test, y_pred = fr_2$mean)

model_3 <- arima(ts_train, order = c(2,1,4))

fr_3 <- forecast.Arima(model_3, h = 60)
r_3 <- R2_Score(y_true = ts_test, y_pred = fr_3$mean)

# Akaike's criterion
AIC(model_1, model_2, model_3) # model_2

# Testing and prediction
plot.forecast(fr_2)
lines(ts_test, col = 'red')