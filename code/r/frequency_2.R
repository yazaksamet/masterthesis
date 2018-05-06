setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
library("ggplot2")
#completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")

blockData$calculated_anomaly = 0
corrList = data.frame(matrix(ncol = 4, nrow = 0))
colnames(corrList) <- c("counter", "startIndex", "corr", "st")

periodSize = 24
startIndex = 28000#1
endIndex = 29000#dim(completeData)[1]
counter = 0
minCorrelation = 0.6

avgCorr = mean(blockData$bestCorr)

while (startIndex < endIndex) {
  counter = counter + 1
  frame1 = blockData[blockData$timestamp < (startIndex + periodSize) & blockData$timestamp >= startIndex,]
  frame2 = blockData[blockData$timestamp < (startIndex + periodSize + periodSize) & blockData$timestamp >= (startIndex + periodSize),]
  
  if (dim(frame1)[1] == dim(frame2)[1]) {
    corr = cor(frame1$value, frame2$value)
    st = sd(frame1$value)
    corrList = rbind(corrList, data.frame(counter, startIndex, corr, st))
    
    if (corr < minCorrelation) {
      for (i in 1:dim(frame1)[1]) {
        frame3 = data.frame(frame1)
        frame4 = data.frame(frame2)
        
        frame3[i, "value"] = 0
        frame4[i, "value"] = 0
        
        corr = cor(frame3$value, frame4$value)    
        if (corr >= avgCorr) {
          blockData[blockData$timestamp == frame4[i, "timestamp"], "calculated_anomaly"] = 1
        }
      }
      
    }
  }
  startIndex = startIndex + periodSize
}

periodicAnomaly = blockData[blockData$calculated_anomaly == 1,]

ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
  geom_point(data = blockPeriodicRecords, aes(x = timestamp, y = value, colour = "periodic")) + 
  geom_point(data = blockAnomaly, aes(x = timestamp, y = value, colour = "anomaly")) + 
  geom_point(data = periodicAnomaly, aes(x = timestamp, y = value, colour = "periodic anomaly")) + 
  ylab('Count')