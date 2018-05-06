setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
library("ggplot2")

completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")
anomalyRecords = completeData[completeData$is_anomaly == 1,]
blockData = completeData[completeData$timestamp <= 1000 & completeData$timestamp >= 0,]
blockAnomaly = blockData[blockData$is_anomaly == 1,]

variance = 1e-8
dataCount = dim(blockData)[1]
posterior = matrix(0, nrow = dataCount, ncol = 1)
errorPost = matrix(0, nrow = dataCount, ncol = 1)
priori = matrix(0, nrow = dataCount, ncol = 1)
Pminus = matrix(0, nrow = dataCount, ncol = 1)
gain = matrix(0, nrow = dataCount, ncol = 1)

measureVariance = 0.001
errorPost[1] = 1.0

for (i in 2:dataCount) {
  priori[i] = posterior[i-1]
  Pminus[i] = errorPost[i-1] + variance
  
  gain[i] = Pminus[i] / ( Pminus[i] + measureVariance )
  posterior[i] = priori[i] + gain[i] * (completeData[i, "value"] - priori[i])
  errorPost[i] = ( 1 - gain[i] ) * Pminus[i]
}

blockData$kalman_value_post = posterior
blockData$kalman_value_pri = priori

ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original Signal")) +
  geom_line(data = blockData, aes(x = timestamp, y = kalman_value_post,   colour = "Kalman Filter"))  +
  #geom_point(data = blockAnomaly, aes(x = timestamp, y = value, colour = "anomaly")) + 
  theme(text = element_text(size = 15))+
  ggtitle("q = 1e-3, v = 1e-8") + 
  ylab('value') +
  xlab('timestamp')