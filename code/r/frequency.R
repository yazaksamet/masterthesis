setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
library("ggplot2")
completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")
completeData = cbind(completeData, isPeriodic = 0, bestPeriod = 0, bestCorr = 0, successiveWindow = 0)

#blockData = completeData[completeData$timestamp <= 62000 & completeData$timestamp >= 61000,]
#blockAnomaly = blockData[blockData$is_anomaly == 1,]

#window1 = completeData[completeData$timestamp <= 61023 & completeData$timestamp >= 61000,]
#window2 = completeData[completeData$timestamp <= 61047 & completeData$timestamp >= 61024,]
# 
# ggplot() +
#   geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
#   #geom_line(data = window1, aes(x = timestamp, y = value,   colour = "w1"))  +
#   #geom_line(data = window2, aes(x = timestamp, y = value,   colour = "w2"))  +
#   geom_point(data = blockAnomaly, aes(x = timestamp, y = value, colour = "anomaly")) + 
#   ylab('Count')

#testCcf = ccf(x = window1$value, y = window2$value, type = c("correlation", "covariance"))
#acf(blockData$value)

maxPeriodSize = 40
innerWindowSize = 10
minCorrelation = 0.6
minSuccesivePeriod = 5

isPeriodic <- function (blockData) {
  windowSize = dim(blockData)[1]
  innerWindowSize = 10
  
  isPeriodic = 0
  corrList = data.frame(matrix(ncol = 2, nrow = 0))
  colnames(corrList) <- c("frameSize", "corr")
  maxTime = max(blockData$timestamp)
  minTime = min(blockData$timestamp)
  
  while (innerWindowSize <= maxPeriodSize) {
    frame1End = maxTime - innerWindowSize
    frame2End = maxTime - (innerWindowSize * 2)
    frame1 = blockData[blockData$timestamp <= maxTime & blockData$timestamp > frame1End,]
    frame2 = blockData[blockData$timestamp <= frame1End & blockData$timestamp > frame2End,]
    
    corr = cor(frame1$value, frame2$value)
    corrList = rbind(corrList, data.frame(innerWindowSize, corr))
    innerWindowSize = innerWindowSize + 1
  }
  
  bestPeriod = corrList[which.max(corrList$corr),]['innerWindowSize'][1,1]
  bestCorr = corrList[which.max(corrList$corr),]['corr'][1,1]
  successiveWindowCount = 0
  
  if (!is.na(bestPeriod) && bestPeriod >= minCorrelation && (windowSize >= minSuccesivePeriod * bestPeriod)) {
    periodCorr = data.frame(matrix(ncol = 3, nrow = 0))
    counter = 1
    
    i = maxTime
    while (i > minTime) {
      
      frame1End = i - bestPeriod
      frame2End = i - (bestPeriod * 2)
      frame1 = blockData[blockData$timestamp <= i & blockData$timestamp > frame1End,]
      frame2 = blockData[blockData$timestamp <= frame1End & blockData$timestamp > frame2End,]
      
      if (dim(frame1)[1] == dim(frame2)[1]) {
        corr = cor(frame1$value, frame2$value)
        periodCorr = rbind(periodCorr, data.frame(i, corr, counter))
      }
      
      i = i - bestPeriod
      counter = counter + 1
    }
    
    successiveWindows = periodCorr[periodCorr$counter <= minSuccesivePeriod & periodCorr$corr >= minCorrelation,]
    successiveWindowCount = dim(successiveWindows)[1]
    if (dim(successiveWindows)[1] == minSuccesivePeriod) {
      isPeriodic = 1
    }
  }
  
  return (data.frame(isPeriodic, bestPeriod, bestCorr, successiveWindowCount));
}

print("Period Start:")
print(date())

windowSizeStart = 200
windowSizeEnd = 200

customStart = 51000
customEnd = 54000

for (windowSize in seq(windowSizeStart, windowSizeEnd, 50)) {
  startIndex = (windowSize+1)
  endIndex = dim(completeData)[1]
  
  windowEndIndex = windowSize
  windowStartIndex = 1
  
  windowRecords = completeData[completeData$timestamp >= windowStartIndex & completeData$timestamp <= windowEndIndex,];
  
  for (i in startIndex:endIndex) {
    if (i %% 3000 == 0) {
      print(paste0("i:", i))
    }
    
    if (i != startIndex) {
      windowRecords = rbind(windowRecords[windowRecords$timestamp > (i-windowSize-1),], completeData[i-1,])
    }
    
    if ((customStart == 0 | i >= customStart) & (customEnd == 0 | i <= customEnd)) {
      periodicResult = isPeriodic(windowRecords);
      completeData[i,"isPeriodic"] = periodicResult[1,1];
      
      if (!is.na(periodicResult[1,2])) {
        completeData[i,"bestPeriod"] = periodicResult[1,2]
      }
      
      if (!is.na(periodicResult[1,3])) {
        completeData[i,"bestCorr"] = periodicResult[1,3]
      }
      
      if (!is.na(periodicResult[1,4])) {
        completeData[i,"successiveWindow"] = periodicResult[1,4]
      }
    }
  }
}

print("Period End:")
print(date())


for (i in 100:dim(completeData)[1]) {
  if (completeData[i, "isPeriodic"] == 1 && completeData[i-1, "isPeriodic"] == 0) {
    periodMargin = completeData[i, "bestPeriod"] * minSuccesivePeriod
    periodStart = i - periodMargin
    completeData[completeData$timestamp >= periodStart & completeData$timestamp <= i, "isPeriodic"] = 1
  }
}



#23500-25000
#26500-28000
#28000-29000
#44000-45000
#61000-62000
#65000-66000

blockData = completeData[completeData$timestamp <= customEnd & completeData$timestamp >= customStart,]
blockPeriodicRecords = blockData[blockData$isPeriodic == 1,]
blockAnomaly = blockData[blockData$is_anomaly == 1,]

#specialPoints = completeData[completeData$timestamp == 65767 | completeData$timestamp == 65718,]

 ggplot() +
   geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
   geom_point(data = blockAnomaly, aes(x = timestamp, y = value, colour = "Anomaly")) + 
   geom_point(data = blockPeriodicRecords, aes(x = timestamp, y = value, colour = "periodic")) + 
   theme(text = element_text(size = 20))+
   ylab('Count') +
   xlab('Timestamp')
 

 
 #write.csv(completeData, "..\\..\\data\\workspace\\real_full_standard_periodic_2.csv")
 
 