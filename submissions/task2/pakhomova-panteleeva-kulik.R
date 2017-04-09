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