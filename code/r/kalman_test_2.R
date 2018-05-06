windowSizeStart = 200
windowSizeEnd = 200

measureVariance = 0.001
variance = 1e-7
intervalMultiplier = 80 # it is used to adjust model success

minConf = 5
maxConf = 15

standardDeviation = 0
mn = 0
diff = 0
predictedValue = 0

resultSet = data.frame(matrix(ncol = 8, nrow = 0))
colnames(resultSet) <- c("windowSize", "intervalMultiplier", "variance", "measureVariance", "tp", "fp", "tn", "fn")

setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard_periodic.csv", header=TRUE, sep=",")
completeData = cbind(completeData, predicted_value = 0, conf_low = 0, conf_high = 0, predicted_anomaly = 0)
completeData$calculation_value = completeData$value

getConfidenceInterval <- function(windowSize, windowRecords) {
  predictionSize = windowSize
  posterior = matrix(0, nrow = predictionSize, ncol = 1)
  errorPost = matrix(0, nrow = predictionSize, ncol = 1)
  priori = matrix(0, nrow = predictionSize, ncol = 1)
  Pminus = matrix(0, nrow = predictionSize, ncol = 1)
  gain = matrix(0, nrow = predictionSize, ncol = 1)
  
  errorPost[1] = 1.0
  
  for (i in 2:predictionSize) {
    priori[i] = posterior[i-1]
    Pminus[i] = errorPost[i-1] + variance
    
    gain[i] = Pminus[i] / ( Pminus[i] + measureVariance )
    posterior[i] = priori[i] + gain[i] * (windowRecords[i, "calculation_value"] - priori[i])
    errorPost[i] = ( 1 - gain[i] ) * Pminus[i]
  }
  
  
  standardDeviation = sd(windowRecords$calculation_value)
  mn = mean(posterior)
  diff = (intervalMultiplier*standardDeviation)/sqrt(windowSize)
  predictedValue = posterior[predictionSize]
  
  if (diff < minConf) {
    diff = minConf
  } else if (diff > maxConf) {
    diff = maxConf
  }
  
  return (data.frame(mn-diff, mn+diff, predictedValue))
}


print("Prediction Start:")
print(date())

for (windowSize in seq(windowSizeStart, windowSizeEnd, 50)) {
  print(paste0("windowSize:", windowSize))
  tp = 0; tn = 0; fp = 0; fn = 0
  
  startIndex = (windowSize+1)
  endIndex = dim(completeData)[1]
  
  windowEndIndex = windowSize
  windowStartIndex = 1
  
  windowRecords = completeData[completeData$timestamp >= windowStartIndex & completeData$timestamp <= windowEndIndex,];
  
  for (i in startIndex:endIndex) {
    if (i %% 3000 == 0) {
      print(paste0("i:", i)) 
      print(paste0("tp:", tp, ", fp:", fp, ", tn:", tn, ", fn:", fn))
    }
    
    completeData[i,"predicted_value"] = 0
    completeData[i,"conf_low"] = 0
    completeData[i,"conf_high"] = 0
    completeData[i,"predicted_anomaly"] = 0
    
    if (i != startIndex) {
      windowRecords = rbind(windowRecords[windowRecords$timestamp > (i-windowSize-1),], completeData[i-1,])
    }
    
    windowForecast = getConfidenceInterval(windowSize, windowRecords);
    
    completeData[i,"predicted_value"] = windowForecast[1,3]
    completeData[i,"conf_low"] = windowForecast[1,1]
    completeData[i,"conf_high"] = windowForecast[1,2]
    
    predictedAnomaly = 
      if ((completeData[i,"value"] <= completeData[i,"conf_high"] 
          & completeData[i,"value"] >= completeData[i,"conf_low"])
          #| completeData[i,"isPeriodic"] == 1
          ) 0 else 1
    
    completeData[i,"predicted_anomaly"] = predictedAnomaly
    
    if (predictedAnomaly == 1) {
      completeData[i,"calculation_value"] = windowForecast[1,3]#(completeData[i,"conf_high"] + completeData[i,"conf_low"])/2
    }
    
    if (completeData[i,"is_anomaly"] == 1 & predictedAnomaly == 1) {
      tp = tp + 1;
    } else if (completeData[i,"is_anomaly"] == 1 & predictedAnomaly == 0) {
      fn = fn + 1
    } else if (completeData[i,"is_anomaly"] == 0 & predictedAnomaly == 1) {
      fp = fp + 1
    } else if (completeData[i,"is_anomaly"] == 0 & predictedAnomaly == 0) {
      tn = tn + 1
    }
  }
  
  resultSet = rbind(resultSet, data.frame(windowSize, intervalMultiplier, variance, measureVariance, tp, fp, tn, fn))
}


 recall = (tp / (tp + fn))
 precision = tp / (tp + fp)
 f1 = 2 * (precision * recall) / (precision + recall)

 periodic_anomaly = completeData[completeData$is_anomaly == 1 & completeData$predicted_anomaly == 0
                                 & (completeData$conf_high < completeData$value | completeData$conf_low > completeData$value),]

 blockData = completeData[completeData$timestamp <= 99000,]
 #plotData = blockData[blockData$timestamp <= 27000 & blockData$timestamp >= 26000,]
 plotData = blockData[blockData$timestamp <= 66000 & blockData$timestamp >= 65000,]

 fpData = plotData[plotData$is_anomaly == 0 & plotData$predicted_anomaly == 1,]
 fnData = plotData[plotData$is_anomaly == 1 & plotData$predicted_anomaly == 0,]
 tpData = plotData[plotData$is_anomaly == 1 & plotData$predicted_anomaly == 1,]
 anomaly_points = plotData[plotData$is_anomaly == 1,]
 periodic_points = plotData[plotData$isPeriodic == 1,]

 periodic_anomaly = plotData[plotData$is_anomaly == 1 & plotData$predicted_anomaly == 0
                                 & (plotData$conf_high < plotData$value | plotData$conf_low > plotData$value),]


 #blockData$ma_value = ma(blockData$value, order=15)

 ggplot() +
   geom_line(data = plotData, aes(x = timestamp, y = value, colour = "Original")) +
   geom_line(data = plotData, aes(x = timestamp, y = conf_low, colour = "conf_low_exp"))  +
   geom_line(data = plotData, aes(x = timestamp, y = conf_high, colour = "conf_high_exp"))  +
   geom_line(data = plotData, aes(x = timestamp, y = predicted_value, colour = "predicted_value"))  +
   #geom_line(data = plotData, aes(x = timestamp, y = calculation_value, colour = "calculation_value"))  +
   geom_point(data = fnData, aes(x = timestamp, y = value, colour = "fn")) +
   geom_point(data = periodic_points, aes(x = timestamp, y = value, colour = "periodic")) +
   #geom_point(data = periodic_anomaly, aes(x = timestamp, y = value, colour = "periodic false")) +
   #geom_point(data = fpData, aes(x = timestamp, y = value, colour = "fp")) +
   #geom_point(data = tpData, aes(x = timestamp, y = value, colour = "tp")) +
   ylab('Count')

#write.csv(resultSet, "kalman_periodic_result_2.csv")


