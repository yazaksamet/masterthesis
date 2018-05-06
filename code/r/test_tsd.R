completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard_periodic_2.csv", header=TRUE, sep=",")
completeData$predicted_anomaly = 0
completeData$low = 0
completeData$high = 0
completeData$seasonLength = 0



completeData$trend = runmed(completeData$value, 23)
blockData = completeData[completeData$timestamp <= 18000 & completeData$timestamp > 17500,]

stlResult = stl(ts(blockData$value, frequency = 24), "periodic")
decomposeResult = decompose(ts(blockData$value, frequency = 24), "additive")
plot(stlResult)
plot(decomposeResult)

periodicRecords = blockData[blockData$isPeriodic == 1,]

detrend = blockData$value - blockData$trend

if (dim(periodicRecords)[1] > 0) {
  m = t(matrix(data = detrend, nrow = 24))
  seasonal = colMedians(m, na.rm = T)
  random = detrend - seasonal
  rm_random = runmed(random[!is.na(random)], 5)
} else {
  random = detrend
  rm_random = runmed(random[!is.na(random)], 5)
}

blockData$rm_random = rm_random
blockData$random = random

std = sd(rm_random, na.rm = T)

if (std < 0.5) {
  std = 0.5
}

blockData$min = median(rm_random, na.rm = T) - 4*std
blockData$max = median(rm_random, na.rm = T) + 4*std

if (dim(periodicRecords)[1] > 0) {
  blockData$high = (blockData$trend + seasonal) + blockData$max
  blockData$low = (blockData$trend + seasonal) + blockData$min
} else {
  blockData$high = blockData$max
  blockData$low = blockData$min
}

ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original")) +
  #geom_line(data = blockData, aes(x = timestamp, y = high, colour = "high")) +
  #geom_line(data = blockData, aes(x = timestamp, y = low, colour = "low")) +
  #geom_line(data = blockData, aes(x = timestamp, y = rm_random, colour = "rm_random")) +
  #geom_line(data = blockData, aes(x = timestamp, y = trend, colour = "trend")) +
  
  #geom_point(data = fp, aes(x = timestamp, y = value, colour = "fp")) + 
  
  #geom_point(data = blockPeriodicRecords, aes(x = timestamp, y = value, colour = "periodic")) + 
  theme(text = element_text(size = 20))+
  ylab('Count') +
  xlab('Timestamp')


#blockData = completeData[completeData$timestamp <= 2000 & completeData$timestamp > 1500,]
#stlRes = stl(ts(blockData$value, frequency = 24), t.window=NULL, s.window="periodic", robust=TRUE)
#plot(stlRes)

