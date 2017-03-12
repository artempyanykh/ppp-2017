library(TTR)
library(tseries)
library(forecast)

#reading data from the file "ppptask1.csv"
pppdata.01 <- read.table("/Users/Anastasia/Desktop/ppptask1.csv", header = T, sep = ",")

#-ITEM 1 --------------------------------------------------------------------------------------

#Common parameters of time series
summary(pppdata.01)

#plot of the time series
plot(pppdata.01, xlab="Data", ylab="Value")

#polynomial smoother СГЛАЖИВАНИЕ ПОКАЗЫВАЕТ, ЧТО ФУНКЦИЯ РЯД ПОСТОЯННО РАСТЕТ => МАТОЖИДАНИЕ НЕ CONST => РЯД НЕ СТАЦИОНАРНЫЙ
lines(lowess(pppdata.01), col="red")

#https://ru.wikipedia.org/wiki/Скользящая_средняя#.D0.9F.D1.80.D0.BE.D1.81.D1.82.D0.BE.D0.B5_.D1.81.D0.BA.D0.BE.D0.BB.D1.8C.D0.B7.D1.8F.D1.89.D0.B5.D0.B5_.D1.81.D1.80.D0.B5.D0.B4.D0.BD.D0.B5.D0.B5
#time series smoothing with Simple Moving Average ПРОСТО ГРАФИК СРЕДНИХ АРИФМЕТИЧЕСКИХ 5-ТИ ЧЛЕНОВ РЯДА
sma.pppdata.01 <- SMA(x = pppdata.01$Value, 5)
#adding SMA line
lines(sma.pppdata.01, col = "green")

#time series smoothing with Weighted Moving Average СУТЬ ТА ЖЕ, ТОЛЬКО МОДЕЛЬ С ВЕСАМИ, ПОСЛЕДНИЕ ЧЛЕНЫ БОЛЕЕ ЗНАЧИМЫ
wma.pppdata.01 <- WMA(x = pppdata.01$Value, 5)
#adding WMA line
lines(wma.pppdata.01, col = "blue")

#time series smoothing with Exponential Moving Average
ema.pppdata.01 <- EMA(x = pppdata.01$Value, 5)
#adding SMA line
lines(ema.pppdata.01, col = "red")

#Dickey-Fuller Test
pppdata.02 <- ts(data = pppdata.01[ ,2], frequency = 12, start = c(1959-01-01))
# не стационарный ЗАЧЕМ ЭТО К, В ЧЕМ ОТЛИЧИЕ С ЛАГАМИ ИЛИ БЕЗ
adf.test(pppdata.02, alternative=c('stationary')) 
adf.test(pppdata.02, alternative=c('stationary'), k=0)
## стацинарный, различие в diff
adf.test(diff(pppdata.02), alternative=c('stationary'))
adf.test(diff(pppdata.02), alternative=c('stationary'), k=0)


#-ITEM 2 --------------------------------------------------------------------------------------

#Trend, Seasons and Random for "multiplicative" and "additive" models
plot(decompose(pppdata.02, type="multiplicative"))
plot(decompose(pppdata.02, type="additive"))

#plots

# Separation of components: Trend, Seasons and Random for "additive" model

plot(pppdata.02)
decompose_add_pppdata.02 = decompose(pppdata.02, "additive")
test <- as.ts(decompose_add_pppdata.02$random)
lines(as.ts(decompose_add_pppdata.02$trend))
lines(as.ts(decompose_add_pppdata.02$seasonal), col = "green")
lines(as.ts(decompose_add_pppdata.02$random))

adf.test(as.ts(decompose_add_pppdata.02$seasonal), alternative=c('stationary')) 
adf.test(as.ts(decompose_add_pppdata.02$random), alternative=c('stationary')) 

# Separation of components: Trend, Seasons and Random for "multiplicative" model

decompose_mult_pppdata.02 = decompose(pppdata.02, "multiplicative")
lines(as.ts(decompose_mult_pppdata.02$trend))
lines(as.ts(decompose_mult_pppdata.02$seasonal), col = "green")
lines(as.ts(decompose_mult_pppdata.02$random))

adf.test(as.ts(decompose_mult_pppdata.02$seasonal), alternative=c('stationary')) 
adf.test(as.ts(decompose_mult_pppdata.02$random), alternative=c('stationary')) 

#STL: Trend, Seasons and Remainder

stl_pppdata.02 = stl(pppdata.02, s.window = "periodic")

plot(stl_pppdata.02)
seasonal_stl_pppdata.02  <- stl_pppdata.02$time.series[,1]
trend_stl_pppdata.02    <- stl_pppdata.02$time.series[,2]
random_stl_pppdata.02  <- stl_pppdata.02$time.series[,3] # 	random???????

adf.test(random_stl_pppdata.02, alternative=c('stationary')) 


plot(pppdata.02)
lines(as.ts(seasonal_stl_pppdata.02))
lines(trend_stl_pppdata.02)
lines(random_stl_pppdata.02)

#-ITEM 3 --------------------------------------------------------------------------------------

#Test for the integrability order САМ РЯД НЕ СТАЦИОНАРНЫЙ, А РАЗНОСТЬ ПЕРВОГО ПОРЯДКА - СТАЦИОНАРНЫЙ РЯД, ЗНАЧИТ РЯЛ ИНТЕГРИРУЕМ 1 ПОРЯДКА
adf.test(pppdata.02, alternative=c('stationary')) # non stationary
adf.test(diff(pppdata.02), alternative=c('stationary')) # stationary

#Arima model НУЖНО ПОДОБРАТЬ МОДЕЛИ ИСХОДЯ ИЗ ГРАФИКОВ
Acf(pppdata.02) #акф функция
Pacf(pppdata.02) #частичная акф
tsdisplay(pppdata.02) #Композиция из ts acf Pacf
tsdisplay(diff(pppdata.02))

mod_1 <- Arima(pppdata.02, order = c(0,1,0)) #единичка говорит о том, что ряд не стационарный. (1, тк разность первого порядка является стационарным процессом)
prognoz <- forecast(mod_1, h = 60)
plot(prognoz)
AIC(mod_1)
mod_2 <- Arima(pppdata.02, order = c(4,1,0))
prognoz2 <- forecast(mod_2, h = 60)
plot(prognoz2)
AIC(mod_2)

mod_3 <- Arima(pppdata.02, order = c(0,1,2))
prognoz3 <- forecast(mod_2, h = 60)
plot(prognoz3)
AIC(mod_3)

mod_4 <- Arima(pppdata.02, order = c(4,1,2))
prognoz4 <- forecast(mod_4, h = 60)
plot(prognoz4)
AIC(mod_4)



mod_a <- auto.arima(pppdata.02)#лучшая модель по критерию акаика
prognoza <- forecast(mod_a, h = 10)
plot(prognoza)
AIC(mod_a)



ppptesting.01 <- read.table("/Users/Anastasia/Desktop/testing.csv", header = T)
plot(ppptesting.01, style = "l")

ppptesting.02 <- ts(data = pppdata.01[ ,2], frequency = 12, start = c(1989,01,01))
plot(ppptesting.02, type="l", col=2)

plot(prognoz4) & lines(ppptesting.02, type="l", col=2)

r2_score <- lm(prognoz4$mean~ppptesting.02)
summary(r2_score)



