library(forecast)
library(stats)
library(TSA)
library(FBN)
library(robustbase)
library("ggplot2")
library("tseries")

setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard_periodic.csv", header=TRUE, sep=",")
completeData = completeData[completeData$timestamp <= 8000 & completeData$timestamp > 7750,]



ggplot() +
  geom_line(data = completeData, aes(x = timestamp, y = value, colour = "Original")) +
  theme(text = element_text(size = 20))+
  ylab('Count') +
  xlab('Timestamp')

completeData_diff = diff(completeData$value, differences = 1)

plot(ts(completeData_diff), xlab="Timestamp", ylab="Value")
title(main = "Differenced Data")

adf.test(completeData$value)
adf.test(ts(completeData_diff))

pacf(ts(completeData_diff))

auto.arima(ts(completeData_diff))
