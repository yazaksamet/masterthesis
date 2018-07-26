setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
#install.packages("AnomalyDetection")
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

completeData = read.csv(file="..\\..\\data\\workspace\\synthetic_full_standard.csv", header=TRUE, sep=",")
completeData$twitterRes = 0
#completeData$timestampX = strptime(c("1.1.2000 00:00"), format = "%d.%m.%Y %H:%M", tz = "CET") + completeData$timestamp * 60 * 60
completeData$timestampX = strptime(c("1.1.1900 00:00"), format = "%d.%m.%Y %H:%M", tz = "CET") + completeData$timestamp# * 60 * 60

tp = 0; fn = 0; tn = 0; fp = 0;

for (i in 2:2) {
  print(i)
  fileData = completeData[completeData$file_number == i,]
  
  testData =data.frame(fileData$timestampX, fileData$value)
  colnames(testData) <- c("timestamp", "count")
  
  res = AnomalyDetectionTs(testData, plot=TRUE)
  
  
  anomalies = data.frame(res[["anoms"]][["timestamp"]])
  colnames(anomalies) <- c("timestamp")
  
  fileData$timestampX = as.character(fileData$timestampX)
  anomalies$timestamp = as.character(anomalies$timestamp)
  
  for (i in 1:dim(anomalies)[1]) {
    fileData[fileData$timestampX == anomalies[i,1], "twitterRes"] = 1
  }
  
  tp = tp + dim(fileData[fileData$is_anomaly == 1 & fileData$twitterRes == 1,])[1]
  fn = fn + dim(fileData[fileData$is_anomaly == 1 & fileData$twitterRes == 0,])[1]
  
  fp = fp + dim(fileData[fileData$is_anomaly == 0 & fileData$twitterRes == 1,])[1]
  tn = tn + dim(fileData[fileData$is_anomaly == 0 & fileData$twitterRes == 0,])[1]

}
recall = (tp / (tp+fn))
precision = tp / (tp+fp)

f1Score = 2 * (precision*recall) / (precision+recall)

res$plot