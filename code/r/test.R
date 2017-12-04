setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
source("functions.R")

originalData = read.csv(file="..\\..\\data\\ydata-labeled-time-series-anomalies-v1_0\\A1Benchmark\\real_1.csv", header=TRUE, sep=",")
tsData = ts(originalData$value)

dailyData = data.frame(originalData, as.integer(originalData$timestamp/24), originalData$timestamp%%24, 0, 0, 0, 0, 0, 0)
##dailyData = data.frame(originalData, as.integer(as.integer(originalData$timestamp/3600)/24), as.integer(originalData$timestamp/3600)%%24, 0, 0, 0, 0, 0, 0)
colnames(dailyData) <- c("TimeStamp","Value","IsAnomaly", "Day", "Hour", "K1", "K2", "K3", "K4", "K5", "K6")

anomalyRecords = dailyData[dailyData$IsAnomaly == 1,];

plot.ts(tsData)
points(data.frame(anomalyRecords$TimeStamp, anomalyRecords$Value), col="red", pch=17)

KN = 2

for (i in 1:dim(dailyData)[1]) {
  currentHour = dailyData[i,"Hour"];
  currentValue = dailyData[i,"Value"];
  currentTimeStamp = dailyData[i,"TimeStamp"];
  
  allDaysHourData = dailyData[dailyData$Hour == currentHour & dailyData$TimeStamp != currentTimeStamp,];
  allDaysHourData["Distance"] = 0
  
  for (d in 1:dim(allDaysHourData)[1]) {
    allDaysHourData[d,"Distance"] = eucledeanDistance(currentValue, allDaysHourData[d, "Value"])
  }
  
  nearestNeighbours = head(allDaysHourData[order(allDaysHourData$Distance, decreasing = FALSE),], n = KN)
  
  avarageKnValue = sum(nearestNeighbours$Distance) / KN
  knColumn = paste0("K", KN)
  dailyData[i, knColumn] = avarageKnValue
}