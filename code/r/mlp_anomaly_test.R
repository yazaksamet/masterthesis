setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
completeData = read.csv(file="..\\..\\data\\workspace\\synthetic_full_standard.csv", header=TRUE, sep=",")

library(ggplot2)
library(gtable)
library(gridExtra)
library(forecast)
#install.packages("neuralnet")
library(neuralnet)

source("MlpAnomalyHelper.r")
source("seasonality.r")

fileData = completeData[completeData$file_number == 70,]
#blockData = fileData[!fileData$is_anomaly == 1, ]
blockData = fileData

seasonality =  getSeasonality(blockData$value, 0.6, FALSE, c(1))

stlRes = stl(ts(blockData$value, frequency = seasonality[["periodLength"]]), s.window = "periodic", robust = TRUE)
blockData$value = blockData$value - as.numeric(stlRes$time.series[,2])
fileData$value = fileData$value - as.numeric(stlRes$time.series[,2])

#p2 = ggplot() +
#  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original"))

#grid.arrange(p2, nrow=1, ncol=1)

sampleRange = 150
trainEndIndex = round(dim(blockData)[1] / 2.5)
mlpData = getMlpData(sampleRange, blockData[1:trainEndIndex,])

n <- names(mlpData)
f <- as.formula(paste("outputValue ~", paste(n[!n %in% c("outputValue", "i")], collapse = " + ")))
nn <- neuralnet(f, data=mlpData, hidden=c(20,10), linear.output=T ,threshold = 0.01)

#plot(nn)

anomalyRecords = data.frame()
testData = fileData
testData$predictedValue = 0
for (i in (sampleRange+1) : dim(testData)[1]) {
  inputValues = data.frame(testData[(i-sampleRange):(i-1),"value"])
  outputValue = testData[i,"value"]
  
  testRow = data.frame()
  testRow = rbind(testRow, cbind(t(inputValues)))
  
  mlpRes = compute(nn, testRow)
  mlpResOutput = mlpRes[["net.result"]]
  testData[i, "predictedValue"] = mlpResOutput
  if (abs(mlpResOutput - outputValue) > 1) {
    calculatedAnomaly = 1
    isAnomaly = testData[i, "is_anomaly"]
    anomalyRecords = rbind(anomalyRecords, cbind(outputValue, mlpResOutput, calculatedAnomaly, isAnomaly, testData[i, "timestamp"]))
    testData[i, "value"] = mlpResOutput
  } else {
    calculatedAnomaly = 0
  }
}

ggplot() +
  geom_line(data = fileData, aes(x = timestamp, y = value, colour = "Original")) +
  geom_line(data = testData, aes(x = timestamp, y = predictedValue, colour = "Test")) +
  geom_point(data = anomalyRecords, aes(x = V5, y = outputValue, colour = "anom"))


#mlpTestData[testRowNumber, "outputValue"]
#plot(mlpResOutput[,1])
#plot(mlpData$output)
#plot(blockData$value)
#testDiff = data.frame(value = mlpResOutput[,1] - mlpTestData$outputValue)
#predictedAnomalies = testDiff[abs(testDiff$value) > 0.50,]


