setwd("D:/Rita/ppp-2017/ppp-2017/tasks/task2")

data <- read.csv(file = "production-data.csv", header = TRUE)

sum_defect <- 0
sum_produced <- 0
table1 <- c()
table2 <- c()
count <- 0

for (j in 1:6) {
  for (i in 1:50) {
    for (k in 1:(8-j)) {
      sum_defect <- sum_defect + data[k+27*(i-1)+count,5]
    }
    sum_produced <- sum_produced + data[27*(i-1)+1+count,4]
    sum_produced <- sum_produced - sum_defect 
    sum_defect <- 0
  }
  table1 <- append(table1, sum_produced)
  sum_produced <- 0
  count <- count + (8-j)
}

count <- 0

for (j in 1:6) {
  for (i in 51:100) {
    for (k in 1:(8-j)) {
      sum_defect <- sum_defect + data[k+27*(i-1)+count,5]
    }
    sum_produced <- sum_produced + data[27*(i-1)+1+count,4]
    sum_produced <- sum_produced - sum_defect 
    sum_defect <- 0
  }
  table2 <- append(table2, sum_produced)
  sum_produced <- 0
  count <- count + (8-j)
}

big <- c()
Companies <- c("Harpy", "Harpy", "Harpy", "Harpy", "Harpy", "Harpy", "Westeros", "Westeros", "Westeros", "Westeros", "Westeros", "Westeros")
Month <- c(table1, table2)

boxplot(Month ~ Companies, col=c("purple", "green"))

produced <- c(data$produced[data$produced > 0])
produced
monthDefects <-c (0,0,0,0,0,0)
monthDefectsMax <- monthDefects
monthDefectsMin <- c(10,10,10,10,10,10)

for (i in 1:50) {
  monthIndex = 0;
  for (j in 1:6) {
    monthLength = (8-j)
    monthProduced = produced[(i-1)*6 + j]
    for (k in 2:monthLength) {
      relDefects = data$defects[(i-1)*27 + monthIndex + k] / monthProduced
      monthDefects[k-1] = monthDefects[k-1] + relDefects
      if (relDefects > monthDefectsMax[k-1])
        monthDefectsMax[k-1] = relDefects
      if (relDefects < monthDefectsMin[k-1])
        monthDefectsMin[k-1] = relDefects
    }
    monthIndex = monthIndex + monthLength
  }
}

for (i in 1:6)
  monthDefects[i] = monthDefects[i]/(50*(7-i))
monthDefectsMin
monthDefectsMax
monthDefects
monthDefects.co <- monthDefects

monthDefects <-c (0,0,0,0,0,0)
monthDefectsMax <- monthDefects
monthDefectsMin <- c(10,10,10,10,10,10)

for (i in 51:100) {
  monthIndex = 0
  for (j in 1:6) {
    monthLength = (8-j)
    monthProduced = produced[(i-1)*6 + j]
    for (k in 2:monthLength) {
      relDefects = data$defects[(i-1)*27 + monthIndex + k] / monthProduced
      monthDefects[k-1] = monthDefects[k-1] + relDefects
      if (relDefects > monthDefectsMax[k-1])
        monthDefectsMax[k-1] = relDefects
      if (relDefects < monthDefectsMin[k-1])
        monthDefectsMin[k-1] = relDefects
    }
    monthDataIndex = monthIndex + monthLength
  }
}
for (i in 1:6)
  monthDefects[i] = monthDefects[i]/(50*(7-i));
monthDefectsMin
monthDefectsMax
monthDefects
monthDefects.inc<-monthDefects

month <- c(1:6)
plot(month, monthDefects.co, col="blue", type="l", ylab="monthDefects")
lines(monthDefects.inc, col="red")