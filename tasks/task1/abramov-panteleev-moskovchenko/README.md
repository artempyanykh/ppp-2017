#Task1

##Проверить является ли ряд стационарным в широком смысле(выполнял Пантелеев):

* Считать данные и создать из входных данных временной ряд:

> training <- read.csv("training.csv", header = TRUE, sep = ",")

> TStraining <- ts(data = training$Value,frequency = 12,start = c(1959,01,01))

* Провести визуальную оценку, отрисовав ряд и скользящую статистику:

> SMAtraining <- SMA(TStraining, n = 20)

> SMAtraining <- ts(data = SMAtraining, frequency = 12, start = c(1959,01,01))

> plot(SMAtraining, type="l", col=2)

> lines(TStraining, type="l")

Результат в файле: Data-with-SMA.png

**Вывод: Так как у графика наблюдается явный тренд, и скользящая средняя не константна на различных моментах времени (явное возрастание), то ряд не является стационарным в широком смысле.**

##Разложить временной ряд на тренд, сезональность, остаток в соответствии с аддитивной, мультипликативной моделями(выполнила Московченко).

* Разложим временной ряд на тренд, cезональность и остаток в соответствии с аддитивной моделью.

> decompose = decompose(TStraining)

Результат в файле: Rplot.png

* Разложим временной ряд на тренд, cезональность и остаток в соответствии с мультипликативной моделью.

> decompose_multi = decompose(TStraining, type = c("multi"))

Результат в файле: Rplot_multi.png

* Оценим стационарность получившихся рядов.

**Как можно заметить, графики тренда для аддитивной и мультипликативной моделей не имеет ярких выбросов и имеет ярко выраженный тренд. Из это можно сделать вывод о том, что ряд не является стационарным.**

* Для определения стационарности рядов сезональности и остатка используем тест Дики-Фуллера аналогично предыдущему:
 
Стационарность остатка для аддитивной модели:

        Value of test-statistic is: -9.4794 

        Critical values for test statistics: 
              1pct  5pct 10pct
        tau1 -2.58 -1.95 -1.62

**Так как значение тестовой статистики меньше критических значений, то этот ряд стационарен.**

Стационарность остатка для мультипликативной модели:

        Value of test-statistic is: -0.3732 

        Critical values for test statistics: 
              1pct  5pct 10pct
        tau1 -2.58 -1.95 -1.62

**Так как значение тестовой статистики больше критических значений, то ряд не стационарен.**

Стационарность сезональности для аддитивной модели:

        Value of test-statistic is: -7.5044 

        Critical values for test statistics: 
              1pct  5pct 10pct
        tau1 -2.58 -1.95 -1.62

**Так как значение тестовой статистики больше критических значений, то ряд стационарен.**

Стационарность cезональности для мультипликативной модели:

        Value of test-statistic is: -0.0304 

        Critical values for test statistics: 
              1pct  5pct 10pct
        tau1 -2.58 -1.95 -1.62

**Так как значение тестовой статистики больше критических значений, то ряд не стационарен.**

##Проверить является ли временной ряд интегрированным порядка k. Если является, применить к нему модель ARIMA:

* Вычислить первые разности ряда и проверить ряд на стационарность с помощью теста Дики-Фуллера

> diff <- diff(TStraining, differences = 1)

> plot(diff)

> dftest = ur.df(diff)

> summary(dftest)

* Отрывок из отчета

            Value of test-statistic is: -8.5541 

            Critical values for test statistics: 
  
                    1pct  5pct 10pct

            tau1 -2.58 -1.95 -1.62

**Вывод: так как значение тестовой статистики меньше критических значений, то ряд первых разностей стационарен, следовательно временной ряд является интегрированным порядка 1. Можем применить к нему модель ARIMA**

* Найти параметры p и q для модели ARIMA, для этого построить ACF И PACF

> acf <- acf(x = diff,plot = TRUE)

> pacf <- pacf(x = diff, plot = TRUE)

**Вывод: по коррелограмме ACF acf.png можно определить количество автокорреляционных коэффициентов сильно отличных от 0 => q=4. по коррелограмме PACF pacf.png можно определить количество автокорреляционных коэффициентов сильно отличных от 0 => p=1. Можем построить модель ARIMA c параметрами (1,1,4)**

* Cтроим модель ARIMA(1,1,4) 

> modelARIMA <- arima(TStraining, order=c(1,1,4))

> modelARIMA

     Call:

     arima(x = TStraining, order = c(1, 1, 4))

     Coefficients:

            ar1     ma1     ma2     ma3     ma4
          
           -0.0449  0.4367  0.2819  0.2476  0.2169
      
     s.e.   0.2487  0.2426  0.1012  0.0754  0.0619

     sigma^2 estimated as 0.1152:  log likelihood = -121.65,  aic = 255.31
     

##Отобрать несколько моделей. Предсказать значения для тестовой выборки. Визуализировать их, посчитать r2 score для каждой из моделей.

* Рассмотрим уже полученную модель ARIMA(1,1,4)

> arimaForecast <- forecast.Arima(modelARIMA, h=60)

> plot(arimaForecast)

* Загрузим тестовые данные и посторим графики

> testing <- read.csv("testing.csv", header = TRUE, sep = ",")

> TStesting <- ts(data=testing, frequency = 12 , start = c(1989,01,01))

> lines(TStesting, type="l", col=2)

Результат в файле: model ARIMA forecast.png

* Посчитаем R^2

> r2_score <- lm(arimaForecast$mean~TStesting)

> summary(r2_score)

      Call:
      lm(formula = arimaForecast$mean ~ TStesting)

      Residuals:
            Min        1Q    Median        3Q       Max 
      -0.213611  0.002935  0.006487  0.008047  0.010550 

      Coefficients:
                   Estimate Std. Error t value Pr(>|t|)    
      (Intercept) 64.628746   0.169572 381.130   <2e-16 ***
      TStesting    0.001804   0.002598   0.694     0.49    
      ---
      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

      Residual standard error: 0.03131 on 58 degrees of freedom
      Multiple R-squared:  0.008242,	Adjusted R-squared:  -0.008857 
      F-statistic: 0.482 on 1 and 58 DF,  p-value: 0.4903

* Cтроим модель ES (Exponential Smoothing)

> fc.etc=forecast(TStraining,h=60)

    Call:
     ets(y = object, lambda = lambda, allow.multiplicative.trend = allow.multiplicative.trend) 

    Smoothing parameters:
     alpha = 0.9999 
     beta  = 0.4484 
     phi   = 0.8 

    Initial states:
     l = 22.7872 
     b = -0.0062 

     sigma:  0.3426

      AIC     AICc      BIC 
    1359.671 1359.909 1382.988 

> plot(fc.etc)

* Загрузим тестовые данные и посторим графики

> testing <- read.csv("testing.csv", header = TRUE, sep = ",") 

> TStesting <- ts(data = testing$Value,frequency = 12,start = c(1989,01,01))

> lines(TStesting,type="l",col=2)

Результат в файле: model ETS forecast.png

* Посчитаем R^2

> r2_score <- lm(fc.etc$mean~TStesting)

> summary(r2_score)

     Call:
     lm(formula = fc.etc$mean ~ TStesting)

     Residuals:
          Min       1Q   Median       3Q      Max 
     -0.52022  0.00443  0.03134  0.05843  0.09393 

     Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
     (Intercept) 63.884938   0.613910 104.062   <2e-16 ***
     TStesting    0.018592   0.009405   1.977   0.0528 .  
     ---
     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

     Residual standard error: 0.1134 on 58 degrees of freedom
     Multiple R-squared:  0.06312,	Adjusted R-squared:  0.04697 
     F-statistic: 3.908 on 1 and 58 DF,  p-value: 0.05283

**Вывод: ARIMA: aic = 255.31 R-squared = 0.008242; ETC: aic = 1359.671 R-squared = 0.06312.
По параметру R-squared модель ETC лучше модели ARIMA, но по информационному критерию Акаике модель ARIMA значительно лучше модели ETC. => Модель ARIMA лучше.**

