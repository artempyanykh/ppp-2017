
library(ggplot2)
production_info <- read.csv(
  file = 'C:/Users/Asus/Documents/GitHub/ppp-2017/tasks/task2/production-data.csv',
  header = TRUE,
  sep  = ',',
  dec = '.')

info_harpy <- production_info[production_info$supplier == 'harpy.co', ]
info_westeros <- production_info[production_info$supplier == 'westeros.inc', ]


harpy_unsullens <- unique(info_harpy$unsullen.id)
westeros_unsullens <- unique(info_westeros$unsullen.id)

harpy_info_def <- c()
for (i in harpy_unsullens) {
  harpy_info_i <- info_harpy[info_harpy$unsullen.id == i, ]
  harpy_info_i_produced <- harpy_info_i$produced[harpy_info_i$produced != 0]
  average_harpy_defects <- c()
  for (j in 1:6) {
    temp <- harpy_info_i[harpy_info_i$production.date == harpy_info_i$report.date - j, ]
    average_harpy_defects[j] <- sum(temp$defects * harpy_info_i_produced[1:(7-j)]) / sum(harpy_info_i_produced[1:(7-j)])
  }
  harpy_info_def <- rbind(harpy_info_def, average_harpy_defects)
}

westeros_info_def <- c()
for (i in westeros_unsullens) {
  westeros_info_i <- info_westeros[info_westeros$unsullen.id == i, ]
  westeros_info_i_produced <- westeros_info_i$produced[westeros_info_i$produced != 0]
  average_westeros_defects <- c()
  for (j in 1:6) {
    temp <- westeros_info_i[westeros_info_i$production.date == westeros_info_i$report.date - j, ]
    average_westeros_defects[j] <- sum(temp$defects * westeros_info_i_produced[1:(7-j)]) / sum(westeros_info_i_produced[1:(7-j)])
  }
  westeros_info_def <- rbind(westeros_info_def, average_westeros_defects)
}


westeros_formatted <- c()
harpy_formatted <- c()
for (i in 1:6) {
  temp <- data.frame(
    value = westeros_info_def[, i],
    month = rep(i, length(westeros_info_def[, i]))
  )
  westeros_formatted <- rbind(westeros_formatted, temp)
  
  temp <- data.frame(
    value = harpy_info_def[, i],
    month = rep(i, length(harpy_info_def[, i]))
  )
  harpy_formatted <- rbind(harpy_formatted, temp)
}


# График дл Harpy
plot_harpy <- ggplot(
  data = harpy_formatted, 
  mapping = aes(factor(month), value))
plot_harpy <- plot_harpy + geom_boxplot(col = "black") 
plot_harpy

# График для Westeros
plot_westeros <- ggplot(
  data = westeros_formatted, 
  mapping = aes(factor(month), value))
plot_westeros <- plot_westeros + geom_boxplot(col = "black") 
plot_westeros

