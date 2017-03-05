#reading training data table with two column: Date and Value
trainingData <- read.csv("C:\\Users\\apolushkin\\Desktop\\ÌÃÓ\\ïðàê\\ÏÏÏ\\tasks\\task1\\training.csv", sep = ",")
#drawing chart of the time series
plot(trainingData[[1]], trainingData[[2]])
#simple moving average for the time series