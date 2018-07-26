setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")

library(forecast)
library(stats)
library(TSA)
library(FBN)
library(robustbase)
library(ggplot2)

completeData = read.csv(file="..\\..\\data\\workspace\\synthetic_full_standard.csv", header=TRUE, sep=",")
periodLength = 0

blockData = completeData[completeData$file_number == 36,]
anom = blockData[blockData$is_anomaly == 1,]

if (periodLength > 0) {
  priodPoints = data.frame(seq(blockData[1,"timestamp"], blockData[dim(blockData)[1],"timestamp"], by=periodLength*3600));
  colnames(priodPoints) <- c("timestamp")
  priodPoints$value = blockData[1,"value"]
} else {
  priodPoints = blockData[1,]  
}

ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
  geom_point(data = anom, aes(x = timestamp, y = value, colour = "anom")) +
  geom_point(data = priodPoints, aes(x = timestamp, y = value, colour = "period")) +
  
  #geom_point(data = blockPeriodicRecords, aes(x = timestamp, y = value, colour = "periodic")) + 
  theme(text = element_text(size = 20))+
  ylab('Count') +
  xlab('Timestamp')