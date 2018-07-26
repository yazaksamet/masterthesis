library(forecast)
library(stats)
library(TSA)
library(FBN)
library(robustbase)
library(ggplot2)

setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
completeData = read.csv(file="..\\..\\data\\workspace\\synthetic_full_standard.csv", header=TRUE, sep=",")
completeData$trend = runmed(completeData$value, 30)

###########################################
#Declare functions
###########################################

source("AnomalyTs.r")
source("seasonality.r")

###########################################
# execute
###########################################
tempFrame = data.frame()
fileResults = data.frame()

for (i in 1:100) {
  print(i)
  completeData$predicted_anomaly = 0
  completeData$low = 0
  completeData$high = 0
  completeData$seasonLength = 0
  completeData$isProcessed = 0
  
  blockData = completeData[completeData$file_number == i,]
  #anomalyResultMain = getAnomalyWithStl(blockData, expansion = 8, minStd = 0.50, mincorr = 0.60, 
  #                                      fixedSeasonality = TRUE, seasonlityValues = c(20,21,22,23,24,161,162,163,164,165,166,167,168,169,170,171,172))
  
  anomalyResultMain = getAnomalyWithStl(blockData, expansion = 4, minStd = 0.20, mincorr = 0.60, 
                                        fixedSeasonality = FALSE, seasonlityValues = c(20))
  
  anomalyResult = anomalyResultMain[["anomalyData"]]
  periodLength = anomalyResultMain[["periodLength"]]
  hasSeasonality = anomalyResultMain[["hasSeasonality"]]
  tempFrame = rbind(tempFrame, anomalyResult)
  
  tpf =  dim(anomalyResult[anomalyResult$is_anomaly == 1 & anomalyResult$predicted_anomaly == 1,])[1]
  fnf =  dim(anomalyResult[anomalyResult$is_anomaly == 1 & anomalyResult$predicted_anomaly == 0,])[1]
  
  fpf =  dim(anomalyResult[anomalyResult$is_anomaly == 0 & anomalyResult$predicted_anomaly == 1,])[1]
  tnf =  dim(anomalyResult[anomalyResult$is_anomaly == 0 & anomalyResult$predicted_anomaly == 0,])[1]
  fileResults = rbind(fileResults, c(i, tpf, fnf, fpf, tnf, periodLength, hasSeasonality))
}

colnames(fileResults) <- c("file_number", "tp", "fn", "fp", "tn", "periodLength", "hasSeasonality")

###########################################
#Evaluate result
###########################################

tempFrame[is.na(tempFrame$predicted_anomaly), "predicted_anomaly"] = 0

tp =  dim(tempFrame[tempFrame$is_anomaly == 1 & tempFrame$predicted_anomaly == 1,])[1]
fn =  dim(tempFrame[tempFrame$is_anomaly == 1 & tempFrame$predicted_anomaly == 0,])[1]

fp =  dim(tempFrame[tempFrame$is_anomaly == 0 & tempFrame$predicted_anomaly == 1,])[1]
tn =  dim(tempFrame[tempFrame$is_anomaly == 0 & tempFrame$predicted_anomaly == 0,])[1]

recall = (tp / (tp+fn))
precision = tp / (tp+fp)
f1Score = 2 * (precision*recall) / (precision+recall)

fileResults$recall = (fileResults$tp / (fileResults$tp+fileResults$fn))
fileResults$precision = fileResults$tp / (fileResults$tp+fileResults$fp)
fileResults$f1Score = 2 * (fileResults$precision*fileResults$recall) / (fileResults$precision+fileResults$recall)


#plotBlock(tempFrame)