#---------------------------------------------------------Part 1
library("TTR")

#считываем данные
sourcedata <- read.csv(file="C:/Users/Lenovo/Desktop/training.csv", header=TRUE, sep=",")
plot(sourcedata[[1]], sourcedata[[2]])

#скользящие статистики
smastatistics <- TTR::SMA(x = sourcedata$Value, 20)
lines(x = sourcedata[[1]], y = smastatistics, col = 'Green')

emastatistics <- TTR::EMA(x = sourcedata$Value, 30)
lines(x = sourcedata[[1]], y = emastatistics, col = 'Red')

#тест Дики-Фуллера

adf.test(sourcedata$Value, alternative="stationary")

#------------------------------------------------------------------------- Part2

tsseries <- ts(sourcedata$Value, frequency = 14, start = c(1959, 1)) 

#аддитивная модель
decompadd <- decompose(log(tsseries), type = c("additive")) 
plot(decompadd, col = 'Blue')

adf.test(na.omit(decompadd$trend), alternative="stationary")

adf.test(na.omit(decompadd$seasonal), alternative="stationary")

adf.test(na.omit(decompadd$random), alternative="stationary")

#мультипликативная модель
decompmult = decompose(log(tsseries), type = c("multi"))
plot(decompmult, col = 'Blue')

adf.test(na.omit(decompmult$trend), alternative="stationary")

adf.test(na.omit(decompmult$seasonal), alternative="stationary")

adf.test(na.omit(decompmult$random), alternative="stationary")