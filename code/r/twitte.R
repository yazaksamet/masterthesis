
library(AnomalyDetection)
completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")

completeData = completeData[completeData$timestamp <= 64200 & completeData$timestamp >= 63200,]

completeData$timestampX = strptime(c("1.1.2000 00:00"), format = "%d.%m.%Y %H:%M", tz = "CET") + completeData$timestamp * 60 * 60

testData =data.frame(completeData$timestampX, completeData$value)
colnames(testData) <- c("timestamp", "count")

res = AnomalyDetectionTs(raw_data, plot=TRUE)
res$plot

res = AnomalyDetectionTs(testData, max_anoms=0.3, direction='both', plot=TRUE)
res$plot

completeData$twitterRes = 0
anomalies = data.frame(res[["anoms"]][["timestamp"]])
colnames(anomalies) <- c("timestamp")

completeData$timestampX = as.character(completeData$timestampX)
anomalies$timestamp = as.character(anomalies$timestamp)

for (i in 1:dim(anomalies)[1]) {
  completeData[completeData$timestampX == anomalies[i,1], "twitterRes"] = 1
}

tp =  dim(completeData[completeData$is_anomaly == 1 & completeData$twitterRes == 1,])[1]
fn =  dim(completeData[completeData$is_anomaly == 1 & completeData$twitterRes == 0,])[1]

fp =  dim(completeData[completeData$is_anomaly == 0 & completeData$twitterRes == 1,])[1]
tn =  dim(completeData[completeData$is_anomaly == 0 & completeData$twitterRes == 0,])[1]

recall = (tp / (tp+fn))
precision = tp / (tp+fp)

f1Score = 2 * (precision*recall) / (precision+recall)

