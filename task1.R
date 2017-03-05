#reading training data table with two column: Date and Value
trainingData <- read.csv("C:\\Users\\apolushkin\\Desktop\\ÌÃÓ\\ïðàê\\ÏÏÏ\\tasks\\task1\\training.csv", sep = ",")

#drawing chart of the time series
plot(trainingData[[1]], trainingData[[2]])

#moving averages for the time series
#Connection to the smatr package, which contains SMA creating function
library(TTR)

#Detection SMA vector with parameter = 20 for time series trainingData
smaData <- TTR::SMA(x = trainingData$Value, 20)
#add SMA vector to the chart
lines(x = trainingData[[1]], y = smaData, col = 'Red')

#detected Linear Weighted Average (WMA) vector
wmaData <- TTR::WMA(x = trainingData$Value, 20)
#add WMA vector to the chart
lines(x = trainingData[[1]], y = wmaData, col = 'Green')

#detected exponential moving average (EMA) vecor
emaData <- TTR::EMA(x = trainingData$Value, 20)
#add WMA vector to the chart
lines(x = trainingData[[1]], y = emaData, col = 'Blue')

#detected trend, season, 
timeSeries <- ts(trainingData$Value, frequency = 12, start = c(1959, 1)) 
tsDecomposition <- stl(log(timeSeries), s.window = 7)
plot(tsDecomposition, col = 'Blue')



