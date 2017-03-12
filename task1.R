## 1. Анализ стационарности в широком смысле временного ряда


#reading training data table with two column: Date and Value
trainingData <- read.csv("tasks\\task1\\training.csv", sep = ",")

#drawing chart of the time series
plot(trainingData[[1]], trainingData[[2]])

#moving averages for the time series
#Connection to the smatr package, which contains SMA creating function
library(TTR)

#Detection SMA vector with parameter = 12 for time series trainingData
smaData <- TTR::SMA(x = trainingData$Value, 12)
#add SMA vector to the chart
lines(x = trainingData[[1]], y = smaData, col = 'Red')

#detected Linear Weighted Average (WMA) vector
wmaData <- TTR::WMA(x = trainingData$Value, 12)
#add WMA vector to the chart
lines(x = trainingData[[1]], y = wmaData, col = 'Green')

#detected exponential moving average (EMA) vecor
emaData <- TTR::EMA(x = trainingData$Value, 12)
#add WMA vector to the chart
lines(x = trainingData[[1]], y = emaData, col = 'Blue')

library(tseries)  
adf.test(trainingData$Value, alternative="stationary")

## 2. Разложение временного ряда

timeSeries <- ts(trainingData$Value, frequency = 12, start = c(1959, 1)) 
tsDecomposition <- stl(log(timeSeries), s.window = 7)
plot(tsDecomposition, col = 'Blue') 

### Разложение в соответствии с аддитивной моделью
tsDecomposition <- decompose(log(timeSeries), type = c("additive"))
plot(tsDecomposition, col = 'Blue') 

### Проверка полученных рядов на стационарность (Тесты Дики-Фуллера)
adf.test(na.omit(tsDecomposition$trend), alternative="stationary")

adf.test(na.omit(tsDecomposition$seasonal), alternative="stationary")

adf.test(na.omit(tsDecomposition$random), alternative="stationary")


### Разложение в соответствии с мультипликативной моделью

tsDecompositionMulti <- decompose(log(timeSeries), type = c("multiplicative"))
plot(tsDecompositionMulti, col = 'green')

### Проверка полученных рядов на стационарность (Тесты Дики-Фуллера)
adf.test(na.omit(tsDecompositionMulti$trend), alternative="stationary")

adf.test(na.omit(tsDecompositionMulti$seasonal), alternative="stationary")

adf.test(na.omit(tsDecompositionMulti$random), alternative="stationary")

## Прогнозирование на основе тренировачных данных

### Интегрированность порядка k

test <- diff(trainingData$Value, differences=1)

emaSeries10 <- EMA(test, n = 10)
emaSeries20 <- EMA(test, n = 20)
plot(x = trainingData$Date[-(1:1)], y = test, type = "l")  
lines(x = trainingData$Date[-(1:1)], y = emaSeries10, col = "red", type = "l") 
lines(x = trainingData$Date[-(1:1)], y = emaSeries20, col = "green", type = "l")


adf.test(test)

### Применение модели ARIMA
##Построение автокорреляционной модели
acf(test)
##Построение частичной автокорреляционной модели
pacf(test)
## Анализ данных и получение значений p и q
## Построение моделей ARIMA для полученных p и q
library(forecast)
arimaModel210 <- Arima(trainingData$Value, order=c(2,1,0))
forecast210 <- forecast.Arima(arimaModel210, h=60)
plot.forecast(forecast210)

## Вычисление Коэффициента детерминации

library(MLmetrics)
ArimaTestData <-  read.csv("tasks\\task1\\testing.csv", sep = ",")
R2_Score(forecast210$mean, ArimaTestData$Value)


arimaModel014 <- Arima(trainingData$Value, order=c(0, 1, 4))
forecast014 <- forecast.Arima(arimaModel014, h=60)
plot.forecast(forecast014)


ArimaTestData <-  read.csv("tasks\\task1\\testing.csv", sep = ",")
R2_Score(forecast014$mean, ArimaTestData$Value)

## отбор наилучшей модели с помощью информационного критерия Акаике. 
AIC(arimaModel210, arimaModel014)

## Подключение линейного дрифта (необязательно)
arimaModel014T <- Arima(trainingData$Value, order=c(0, 1, 4), include.drift=TRUE)
forecast014T <- forecast.Arima(arimaModel014T, h=60)
plot.forecast(forecast014T)

ArimaTestData <-  read.csv("tasks\\task1\\testing.csv", sep = ",")
R2_Score(forecast014T$mean, ArimaTestData$Value)


AIC(arimaModel014T)
