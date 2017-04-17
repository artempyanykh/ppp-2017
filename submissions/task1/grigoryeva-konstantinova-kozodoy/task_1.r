install.packages("forecast")
install.packages("MLmetrics")
install.packages("tseries")
install.packages("zoo")
install.packages("xts")
library(forecast)
library(MLmetrics)
library(tseries)
library(zoo)
library(xts)

times_series<-read.csv("training.csv",header=TRUE, row.names = 1)
times_series.ts<-ts(times_series, start=c(1959,1), frequency=12)
times_series.ts
plot.ts(times_series.ts)
#1 способ
adf.test(times_series.ts,alternative="stationary")
#Поскольку значение p-value велико, 
#то нет оснований отвергать гипотезу о наличии единичного корня (ряд можно считать нестационарным).
#2 способ
#Скользящее среднее
times_series_rm<-rollmean(times_series.ts,4)
lines(times_series_rm,col="red")
#Стандартное отклонение
times_series_sd<-((times_series_rm-times_series.ts)^2/359)^(1/2)
lines(times_series_sd+40,col="blue")

#Первый график - временной ряд
#Второй график - оценка составляющей тренда
#Третий график - оценка сезонной компоненты
#Четвертый график - оценка случайной компоненты
type<-c("additive", "multiplicative")
times_series_components<-decompose(times_series.ts,type[1])
plot(times_series_components)
times_series_components<-decompose(times_series.ts,type[2])
plot(times_series_components)

adf.test(times_series_components$seasonal,alternative="stationary")
times_series_components$trend
times_series_components$random

diff_1<-diff(times_series.ts, differences=1)
plot.ts(diff_1)
adf.test(diff_1,alternative="stationary")
#стационарный, так как p-value очень мало
#функция , которая считает порядок интегрирования библиотека forecast
d<-ndiffs(times_series.ts)#d=1
(acf(diff_1,main=""))#q = 4
(pacf(diff_1,main=""))#p = 2

model01<-arima(times_series.ts,order=c(2,1,0))#order(p,d,q)
model02<-arima(times_series.ts,order=c(0,1,4))#order(p,d,q)
model03<-arima(times_series.ts,order=c(2,1,4))#order(p,d,q)
model04<-arima(times_series.ts,order=c(1,1,1))#

#сравниваем модели по r2_score он должен быть ближе к нулю
test<-read.csv("testing.csv",header=TRUE, row.names = 1)
test.ts<-ts(test, start=c(1989,1), frequency=12)
test.ts

frts_1<-forecast.Arima(model01,h=60)
R2_Score(frts_1$mean,test.ts)
plot.forecast(frts_1)
frts_2<-forecast.Arima(model02,h=60)
R2_Score(frts_2$mean,test.ts)
plot.forecast(frts_2)
frts_3<-forecast.Arima(model03,h=60)
R2_Score(frts_3$mean,test.ts)
plot.forecast(frts_3)
frts_4<-forecast.Arima(model04,h=60)
R2_Score(frts_4$mean,test.ts)
plot.forecast(frts_4)
#берем минмимальное:
AIC(model01,model02,model03,model04)
#выбираем модель 2) и выводим все на график
plot.forecast(frts_2)
lines(test.ts,col='red')
frts_2$mean







