library("TTR")
library("forecast")
library(forecast) 
library(MLmetrics)

sourcedata <- read.csv(file="C:/Users/Lenovo/Desktop/training.csv", header=TRUE, sep=",")
testingdata <- read.csv("C:/Users/Lenovo/Desktop/testing.csv", sep = ",") 

dataint <- diff(sourcedata$Value, differences=1) 
adf.test(dataint) 
acf(x=dataint) 
pacf(x=dataint) 

Model1 <- Arima(sourcedata$Value, order=c(0,1,4)) 
Model1forec <- forecast.Arima(Model1, h=60) 
plot.forecast(Model1forec)  
R2_Score(Model1forec$mean, testingdata$Value) 

Model2 <- Arima(sourcedata$Value, order=c(2, 1, 4)) 
Model2forec <- forecast.Arima(Model2, h=60) 
plot.forecast(Model2forec) 
R2_Score(Model2forec$mean, testingdata$Value) 

Model3 <- Arima(sourcedata$Value, order=c(2, 1, 0)) 
Model3forec <- forecast.Arima(Model3, h=60) 
plot.forecast(Model3forec) 
R2_Score(Model3forec$mean, testingdata$Value) 

AIC(Model1, Model2, Model3) 


