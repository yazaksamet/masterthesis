calculateCorrelation <- function(inputData, targetField)
{
  corrMatrix = matrix(0, ncol = length(inputData), nrow = 1)
  
  for (i in 1:length(inputData)) {
    corrMatrix[1,i] = cor(inputData[,i], targetField)  
  }
  
  corrMatrix = data.frame(corrMatrix)
  colnames(corrMatrix) <- colnames(inputData)
  
  return (corrMatrix)
}

getNumericalStats <- function(inputData)
{
  drops <- c("Player", "SeasonEnd", "Season", "Season.Type", "Team", "SeasonStart", "IsAllStar", "Conference")
  inputData = inputData[ , !(names(inputData) %in% drops)]
  return (inputData)
}

getTargetClassStatics <- function(inputData)
{
  return (nrow(inputData[inputData$IsAllStar == 1, ]) / nrow(inputData[inputData$IsAllStar == 0, ]))
}

duplicateLowerData <- function(inputData, minPercentage)
{
  allStarRows = inputData[inputData$IsAllStar == 1, ]
  
  while(getTargetClassStatics(inputData) < minPercentage) {
    inputData = rbind(inputData, allStarRows)  
  }
  
  return (inputData)
}

eucledeanDistance <- function(v1, v2)
{
  instanceDistance = sqrt(sum((as.numeric(v1) - as.numeric(v2))^2))
  return (instanceDistance)
}
