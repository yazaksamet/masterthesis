library(forecast)
library(stats)
library(TSA)
library(FBN)
library(robustbase)

getSeasonalityValue <- function(windowRecords) {
  p = periodogram(windowRecords, plot=FALSE)
  dd = data.frame(freq=p$freq, spec=p$spec)
  order = dd[order(-dd$spec),]
  top2 = head(order, 3)
  top2Window = 1/top2$f
  
  if (isSeasonalityValid(top2Window[1], windowRecords)) {
    return (round(top2Window[1]))
  } else if (isSeasonalityValid(top2Window[2], windowRecords)) {
    return (round(top2Window[2]))
  } else if (isSeasonalityValid(top2Window[3], windowRecords)) {
    return (round(top2Window[3]))
  } else {
    return (0)
  }
}

isSeasonalityValid <- function(seasonalityValue, windowRecords) {
  windowLength = length(windowRecords)
  return (!is.na(seasonalityValue) && seasonalityValue > 23 && seasonalityValue < (27))
}

getAnomalyRecords <- function(blockData) {
  seasonLength = getSeasonalityValue(blockData$value)  
  periodicRecords = blockData[blockData$isPeriodic == 1,]
  
  minPeriod = dim(blockData)[1] * 0.40
  
  if (dim(periodicRecords)[1] > minPeriod && seasonLength > 0) {
    #blockData$trend = runmed(blockData$value, seasonLength)
    
    #decomposed_days = decompose(ts(blockData$value, frequency = top2Window[1]), "multiplicative")
    #plot(decomposed_days)
    
    #blockData$resudial = decomposed_days$random
    
    detrend = blockData$value - blockData$trend
    m = t(matrix(data = detrend, nrow = seasonLength))
    seasonal = colMedians(m, na.rm = T)
    random = detrend -seasonal
    rm_random = runmed(random[!is.na(random)], 5)
  } else {
    #blockData$trend = runmed(blockData$value, 24)
    detrend = blockData$value - blockData$trend
    random = detrend
    rm_random = runmed(random[!is.na(random)], 5)
  }
  
  blockData$rm_random = rm_random
  blockData$random = random
  
  std = mad(rm_random, center = median(rm_random))
  
  #std = sd(rm_random, na.rm = T)
  if (std < 0.2) {
    std = 0.2
  }
  
  blockData$min = mean(rm_random, na.rm = T) - (8)*std
  blockData$max = mean(rm_random, na.rm = T) + (8)*std
  
  if (dim(periodicRecords)[1] > minPeriod && seasonLength > 0) {
    blockData$high = (blockData$trend + seasonal) + blockData$max
    blockData$low = (blockData$trend + seasonal) + blockData$min
    blockData$isProcessed = 1
    
    detectedAnomaly = blockData[blockData$value < blockData$low | blockData$value > blockData$high,]
    blockData[blockData$value < blockData$low | blockData$value > blockData$high, "predicted_anomaly"] = 1
  } else {
    blockData$high = blockData$max
    blockData$low = blockData$min
    
    detectedAnomaly = blockData[blockData$random < blockData$low | blockData$random > blockData$high,]
    blockData[blockData$random < blockData$low | blockData$random > blockData$high, "predicted_anomaly"] = 1
  }
  
  
  blockData$seasonLength = seasonLength
  
  if (dim(detectedAnomaly)[1] > 0) {
    detectedAnomaly$predicted_anomaly = 1
  }
  return (blockData)
}

plotBlock <- function(blockData) {
  anom = blockData[blockData$is_anomaly == 0 & blockData$predicted_anomaly == 1,]
  
  ggplot() +
    geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
    geom_line(data = blockData, aes(x = timestamp, y = high, colour = "high")) +
    geom_line(data = blockData, aes(x = timestamp, y = low, colour = "low")) +
    #geom_line(data = blockData, aes(x = timestamp, y = rm_random, colour = "rm_random")) +
    geom_line(data = blockData, aes(x = timestamp, y = random, colour = "random")) +
    #geom_line(data = blockData, aes(x = timestamp, y = trend, colour = "trend")) +
    #geom_line(data = blockData, aes(x = timestamp, y = max, colour = "max")) +
    
    geom_point(data = anom, aes(x = timestamp, y = value, colour = "anom")) + 
    
    #geom_point(data = blockPeriodicRecords, aes(x = timestamp, y = value, colour = "periodic")) + 
    theme(text = element_text(size = 20))+
    ylab('Count') +
    xlab('Timestamp')
}


completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard_periodic.csv", header=TRUE, sep=",")
completeData$predicted_anomaly = 0
completeData$low = 0
completeData$high = 0
completeData$seasonLength = 0
completeData$isProcessed = 0

completeData$trend = runmed(completeData$value, 23)

#completeData = completeData[completeData$timestamp <= 9000 & completeData$timestamp > 8000,]

windowSize = 24*4
counter = completeData[1, "timestamp"] - 1
endIndex = (counter+windowSize)
blockCounter = 1

tempFrame = data.frame()

while (counter <= max(completeData$timestamp)) {
  
  blockData = completeData[completeData$timestamp <= endIndex & completeData$timestamp > counter,]
  anomalyResult = getAnomalyRecords(blockData)
  
  tempFrame = rbind(tempFrame, anomalyResult[dim(anomalyResult)[1],])
  
  counter = counter + 1
  blockCounter = blockCounter + 1
  endIndex = (endIndex+1)
  
  if (blockCounter %% 1000 == 0) {
    print(paste0("i:", counter)) 
    
    tpf =  dim(tempFrame[tempFrame$is_anomaly == 1 & tempFrame$predicted_anomaly == 1,])[1]
    fnf =  dim(tempFrame[tempFrame$is_anomaly == 1 & tempFrame$predicted_anomaly == 0,])[1]
    
    fpf =  dim(tempFrame[tempFrame$is_anomaly == 0 & tempFrame$predicted_anomaly == 1,])[1]
    tnf =  dim(tempFrame[tempFrame$is_anomaly == 0 & tempFrame$predicted_anomaly == 0,])[1]
    
    print(paste0("tp:", tpf, ", fp:", fpf, ", tn:", tnf, ", fn:", fnf)) 
  }
}

tempFrame[is.na(tempFrame$predicted_anomaly), "predicted_anomaly"] = 0

tp =  dim(tempFrame[tempFrame$is_anomaly == 1 & tempFrame$predicted_anomaly == 1,])[1]
fn =  dim(tempFrame[tempFrame$is_anomaly == 1 & tempFrame$predicted_anomaly == 0,])[1]

fp =  dim(tempFrame[tempFrame$is_anomaly == 0 & tempFrame$predicted_anomaly == 1,])[1]
tn =  dim(tempFrame[tempFrame$is_anomaly == 0 & tempFrame$predicted_anomaly == 0,])[1]

recall = (tp / (tp+fn))
precision = tp / (tp+fp)

f1Score = 2 * (precision*recall) / (precision+recall)


#blockData = completeData[completeData$timestamp < 5000 & completeData$timestamp >= 4000,]

if (dim(tempFrame)[1] < 2002) {
  plotBlock(tempFrame)
}

