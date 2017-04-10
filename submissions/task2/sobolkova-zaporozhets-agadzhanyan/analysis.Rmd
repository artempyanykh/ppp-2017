library(psych)
library(dplyr)
library(ggplot2)
library(qicharts)
library(qcc)
library(GGally)
library(graphics)
library(qualityTools)
library(epiR)


pppdata.01 <- read.table("/Users/Anastasia/Desktop/ppptask2.csv", header = T, sep = "\t")
pppdata.01.westeros <- pppdata.01[pppdata.01$supplier=="westeros.inc",]
pppdata.01.harpy <- pppdata.01[pppdata.01$supplier=="harpy.co",]

glimpse(pppdata.01)

#BoxPlot
boxplot(pppdata.01$defects~pppdata.01$supplier, ylab="Defects", xlab="Suppliers")
boxplot(pppdata.01$produced~pppdata.01$supplier, lab="Produced", xlab="Suppliers")
hist(pppdata.01$defects)

#Histogram
par(mfrow=c(1,2))
hist(pppdata.01$defects, main="Hisogram of Defects")
hist(pppdata.01$produced, main="Histogram of Produced")
dev.off()
qplot(data=pppdata.01, defects, fill=supplier)
qplot(data=pppdata.01, defects, fill=supplier, geom = "density", alpha = 0.5)

#Multi-vari Chart Plot
mvPlot(pppdata.01, pppdata.01$defects, pppdata.01$produced)
mvPlot(pppdata.01$defects, pppdata.01$produced, pppdata.01$supplier)

#RunChart
qic(pppdata.01$defects)

#Pareto
paretoChart(pppdata.01.harpy$defects, main="Harpy.co, Defectes, Pareto chart")
paretoChart(pppdata.01.westeros$defects, main="Westeros.inc, Defectes, Pareto chart")
paretoChart(pppdata.01.harpy$produced, main="Harpy.co, Produced, Pareto chart")
paretoChart(pppdata.01.westeros$produced, main="Westeros.inc, Produced, Pareto chart")

#Scatter Plot
par(mfrow=c(2,2))
plot(pppdata.01.harpy$produced, pppdata.01.harpy$production.date, col=2, ylab="Months", xlab= "Produced weapons in Harpy.co")
plot(pppdata.01.westeros$produced, pppdata.01.westeros$production.date, col=2, ylab="Months", xlab= "Produced weapons in Westeros.inc")
plot(pppdata.01.harpy$production.date, pppdata.01.harpy$defected, col=3, ylab="Months", xlab= "Defected weapons in Harpy.co")
plot(pppdata.01.westeros$production.date, pppdata.01.westeros$defected, col=3, ylab="Months", xlab= "Defected weapons in Westeros.inc")
dev.off()

col=ifelse(pppdata.01$supplier=="harpy.co", "black", "red")
plot(pppdata.01$production.date, pppdata.01$defected, , ylab="Months", xlab= "Produced weapons", pch=ifelse(pppdata.01$supplier=="harpy.co", 1, 2))

#Stem-and-leaf
stem(pppdata.01.westeros$defects)
stem(pppdata.01.harpy$defects)
stem(pppdata.01.harpy$produced)
stem(pppdata.01.westeros$produced)



#OddsRatio

#Multidimensional scaling







#PCA
pppdata.01.westeros$supplier <- NULL
glimpse(pppdata.01.westeros)
pppdata.01.harpy$supplier <- NULL
glimpse(pppdata.01.harpy)


pppdata.01.westeros.pca <- prcomp(pppdata.01.westeros, scale=TRUE)
pppdata.01.harpy.pca <- prcomp(pppdata.01.harpy, scale=TRUE)
#вытащили столбик первых X
w.pca1 <- pppdata.01.westeros.pca$x[,1]
h.pca1 <- pppdata.01.harpy.pca$x[,1]
#веса первой главной компоненты
w.v1 <- pppdata.01.westeros.pca$rotation[,1]
h.v1 <- pppdata.01.harpy.pca$rotation[,1]
#веса, с которыми старые компоненты вошли в новые компоненты
w.v1
h.v1


w.v2 <- pppdata.01.westeros.pca$rotation[,2]
h.v2 <- pppdata.01.harpy.pca$rotation[,2]
#веса, с которыми старые компоненты вошли в новые компоненты
w.v2
h.v2


summary(pppdata.01.westeros.pca)
summary(pppdata.01.harpy.pca) 
plot(pppdata.01.westeros.pca, xlab="Components", main="Explained variance by components, Westeros.inc")
plot(pppdata.01.harpy.pca, xlab="Components", main="Explained variance by components Harpy.co")
biplot(pppdata.01.westeros.pca)
biplot(pppdata.01.harpy.pca)

plot(pppdata.01.westeros$production.date, pppdata.01.westeros$defects, main="Westeros.inc", xlab="production.date",ylab="defects", ylim=c(0, 20))
plot(pppdata.01.harpy$production.date, pppdata.01.harpy$defects, main="Harpy.co", xlab="production.date",ylab="defects")

