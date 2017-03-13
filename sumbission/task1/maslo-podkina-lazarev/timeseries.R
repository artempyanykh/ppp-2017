# read file from current dir (it's default behavior when launched from the command line)
# to change dir ir R Studio Session > Set Working Directory > To Source File Location
timeSeries = read.csv(file="training.csv", header = TRUE,sep=",")
testSeries = read.csv(file="testing.csv", header = TRUE,sep=",")
# include TTR library
library(TTR)
library(tseries)
library(forecast)

# function to draw original data + statistic: SMA, WMA, EMA
# n is order for SMA, WMA, EMA stat funcs
drawStatistics <- function(date, value, n = 20) {
  jpeg()
  # calculating
  # sma 
  SMAValue <- SMA(value, n = n)
  # wma
  WMAValue <- WMA(value, n = n)
  # ema
  EMAValue <- EMA(value, n = n)
  
  # create plot obj
  plot(date, value)
  # visualisation
  # original points
  lines(date, value)
  # SMA
  lines(x = date, y = SMAValue, col = "green")
  # WMA
  lines(x = date, y = WMAValue, col = "red")
  # EMA
  lines(x = date, y = EMAValue, col = "blue")
  
  dev.off()
}

removeNA <- function(value) {
  value[!is.na(value)]
}

DecomposeSeries <- function(date, value) {
  
  series = ts(value, frequency = 12, start = c(1959))
  logSeries = log(series)
  
  #additive model
  addModel <- decompose(logSeries, type = c("additive"))
  #multilplication model
  multModel <- decompose(logSeries, type = c("multiplicative"))
  
  newDate <- head(date, -6)
  jpeg()
  plot(multModel)
  dev.off()
  
  #adf.test(removeNA(addModel$seasonal), alternative="stationary")
  #adf.test(removeNA(addModel$random), alternative="stationary")

  adf.test(removeNA(multModel$seasonal), alternative="stationary")
  adf.test(removeNA(multModel$random), alternative="stationary")
  #adf.test(diff(removeNA(multModel$seasonal)),"stationary")
  #drawStatistics(newDate, head(addModel$seasonal, -6))
  #drawStatistics(newDate, head(addModel$random, -6))
  
  #drawStatistics(newDate, head(multModel$seasonal, -6))
  #drawStatistics(newDate, head(multModel$random, -6))
}

date <- timeSeries$Date
value <- timeSeries$Value

drawStatistics(date, value, 20)
#drawStatistics(head(date, -1), diff(value))

#integrating of order = 1
dif <- diff(value)
adf.test(dif, alternative="stationary")

DecomposeSeries(date, value)
#arima
acf <- acf(x = dif,plot = TRUE)
pacf <- pacf(x = dif, plot = TRUE)

trainSer <- ts(value, frequency = 12, start = c(1959))

arima <- arima(trainSer, order = c(1, 3, 4)) # 3 8
arima

aForecast = forecast.Arima(arima, h = 60)

plot(aForecast)
lines(testSer, col = "green")

M.lm <- lm(aForecast$mean~testSer)
str(summary(M.lm))
AIC(arima)
AIC(arima)