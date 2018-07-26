getSeasonality <- function(tsData, mincorr, fixedSeasonality, seasonlityValues) {
  p = periodogram(tsData, plot=FALSE)
  dd = data.frame(freq=p$freq, spec=p$spec)
  order = dd[order(-dd$spec),]
  topFreq = head(order, 5)
  topWindow = 1/topFreq$f
  topWindowFrame = data.frame(topWindow)
  hasSeasonality = 0
  
  periodLength = round(head(topWindowFrame[topWindowFrame$topWindow < (length(tsData) / 2),1], 1))
  if (periodLength > 0 & periodLength < (length(tsData) / 2) ) {
    hasSeasonality = 1  
  }
  
  topWindowFrame
  
  bestPeriod = periodLength
  bestCorr = 0
  
  for (i in 1:dim(topWindowFrame)[1]) {
    periodLength = round(topWindowFrame[i,1])
    if (periodLength < (length(tsData) / 2)) {
      periodMargin = round(periodLength / 10)
      minPeriod = periodLength - periodMargin
      maxPeriod = periodLength + periodMargin
      
      if (maxPeriod < dim(blockData)[1]) {
        for (i in minPeriod:maxPeriod) {
          m = t(matrix(data = blockData$value, nrow = i))  
          
          if (nrow(m) > 2) {
            currentCor = cor(m[1,], m[2,])
            
            if (currentCor > bestCorr & mincorr <= currentCor) {
              bestCorr = currentCor
              bestPeriod = i
            }
          }
        }
      }
    }
  }
  
  if (fixedSeasonality & !(bestPeriod %in% seasonlityValues)) {
    bestPeriod = 0
    hasSeasonality = 0
  }
  
  return(list(hasSeasonality = hasSeasonality, periodLength = bestPeriod))
}