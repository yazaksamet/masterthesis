getAnomalyWithStl <- function(blockData, expansion, minStd, mincorr, fixedSeasonality, seasonlityValues) {
  seasonalityRes = getSeasonality(blockData$value, mincorr, fixedSeasonality, seasonlityValues)
  
  hasSeasonality = seasonalityRes[["hasSeasonality"]]
  periodLength = seasonalityRes[["periodLength"]]
  
  if (hasSeasonality == 0) {
    return (getAnomalyRecords(blockData, expansion, minStd, mincorr))
  }
  
  stlRes = stl(ts(blockData$value, frequency = round(periodLength)), s.window = "periodic", robust = TRUE)
  blockData$random = as.numeric(stlRes$time.series[,3])
  blockData$rm_random = runmed(blockData$random[!is.na(blockData$random)], 5)
  
  std = mad(blockData$rm_random, center = median(blockData$rm_random))
  
  if (std < minStd) {
    std = minStd
  }
  
  blockData$min = mean(blockData$rm_random, na.rm = T) - (expansion)*std
  blockData$max = mean(blockData$rm_random, na.rm = T) + (expansion)*std
  
  blockData$high = blockData$max
  blockData$low = blockData$min
  
  detectedAnomaly = blockData[blockData$random < blockData$low | blockData$random > blockData$high,]
  blockData[blockData$random < blockData$low | blockData$random > blockData$high, "predicted_anomaly"] = 1
  
  if (dim(detectedAnomaly)[1] > 0) {
    detectedAnomaly$predicted_anomaly = 1
  }
  return(list(anomalyData = data.frame(blockData),hasSeasonality = hasSeasonality, periodLength = periodLength))
}

getAnomalyRecordsWithManual <- function(blockData, expansion, minStd, mincorr, fixedSeasonality, seasonlityValues ) {
  seasonalityRes = getSeasonality(blockData$value, mincorr, fixedSeasonality, seasonlityValues)
  
  hasSeasonality = seasonalityRes[["hasSeasonality"]]
  periodLength = seasonalityRes[["periodLength"]]
  
  if (hasSeasonality == 0) {
    return (getAnomalyRecords(blockData, expansion, mincorr))
  }
  
  detrend = blockData$value - blockData$trend
  m = t(matrix(data = detrend, nrow = periodLength))
  seasonal = colMedians(m, na.rm = T)
  random = detrend -seasonal
  rm_random = runmed(random[!is.na(random)], 5)
  
  blockData$rm_random = rm_random
  blockData$random = random
  
  std = mad(blockData$rm_random, center = median(blockData$rm_random))
  
  if (std < minStd) {
    std = minStd
  }
  
  blockData$min = mean(blockData$rm_random, na.rm = T) - (expansion)*std
  blockData$max = mean(blockData$rm_random, na.rm = T) + (expansion)*std
  
  blockData$high = (blockData$trend + seasonal) + blockData$max
  blockData$low = (blockData$trend + seasonal) + blockData$min
  
  detectedAnomaly = blockData[blockData$value < blockData$low | blockData$value > blockData$high,]
  blockData[blockData$value < blockData$low | blockData$value > blockData$high, "predicted_anomaly"] = 1
  
  if (dim(detectedAnomaly)[1] > 0) {
    detectedAnomaly$predicted_anomaly = 1
  }
  return(list(anomalyData = data.frame(blockData),hasSeasonality = hasSeasonality, periodLength = periodLength))
}

getAnomalyRecords <- function(blockData, expansion, minStd, mincorr) {
  
  detrend = blockData$value - blockData$trend
  random = detrend
  rm_random = runmed(random[!is.na(random)], 5)
  
  blockData$rm_random = rm_random
  blockData$random = random
  
  std = mad(rm_random, center = median(rm_random))
  
  if (std < minStd) {
    std = minStd
  }
  
  blockData$min = mean(rm_random, na.rm = T) - (expansion)*std
  blockData$max = mean(rm_random, na.rm = T) + (expansion)*std
  
  blockData$high = blockData$max
  blockData$low = blockData$min
  
  detectedAnomaly = blockData[blockData$random < blockData$low | blockData$random > blockData$high,]
  blockData[blockData$random < blockData$low | blockData$random > blockData$high, "predicted_anomaly"] = 1
  
  if (dim(detectedAnomaly)[1] > 0) {
    detectedAnomaly$predicted_anomaly = 1
  }
  return(list(anomalyData = data.frame(blockData),hasSeasonality = 0, periodLength = 0))
}

plotBlock <- function(blockData) {
  anom = blockData[blockData$predicted_anomaly == 1,]
  
  if (dim(anom)[1] == 0) {
    anom = blockData[1,]  
  }
  
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