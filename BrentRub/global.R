# rm(list=(ls)())

library(xts)
buff <- read.csv('buff.csv')
buff$date <- as.Date(as.character(buff$date))
first_date <- buff$date[1]
fx <- buff
fx <- xts(buff[!names(buff) == 'date'], order.by = buff$date)
