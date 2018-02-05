setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")

library("forecast")
library("tseries")
library("ggplot2")

completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")
blockData = completeData[completeData$timestamp <= 1215,]
#plot.ts(ts(blockData$value), xlab="Timestamp", ylab="Value")

count_ts = ts(ts(blockData$value))

blockData$clean_value = tsclean(count_ts)

#plot.ts(ts(blockData$clean_value), xlab="Timestamp", ylab="clean_value")

blockData$cnt_ma = ma(blockData$value, order=7) # using the clean count with no outliers
blockData$cnt_ma30 = ma(blockData$value, order=30)
blockData$cnt_ma2 = ma(blockData$value, order=2)
omitted_data = data.frame(na.omit(blockData$cnt_ma2))

ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
  geom_line(data = blockData, aes(x = timestamp, y = clean_value,   colour = "Clean"))  +
  geom_line(data = blockData, aes(x = timestamp, y = cnt_ma, colour = "Weekly Moving Average"))  +
  geom_line(data = blockData, aes(x = timestamp, y = cnt_ma30, colour = "Month Moving Average"))  +
  ylab('Count')


count_ma = ts(na.omit(blockData$clean_value), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
#plot(decomp)


#adf.test(count_ma, alternative = "stationary")

#Acf(count_ma, main='')

#Pacf(count_ma, main='')


#count_d1 = diff(deseasonal_cnt, differences = 1)
#plot(count_d1)
#adf.test(count_d1, alternative = "stationary")

#Acf(count_d1, main='ACF for Differenced Series')
#Pacf(count_d1, main='PACF for Differenced Series')

auto_arima = auto.arima(count_ma, seasonal=FALSE)

#fit2 = arima(deseasonal_cnt, order=c(2,1,2))

fcast <- forecast(auto_arima, h=10, level=c(99.9))
plot(fcast)
fcastRes = data.frame(fcast)
