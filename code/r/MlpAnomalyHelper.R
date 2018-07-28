getMlpData <- function(columnSize, blockData) {
  mlpData = data.frame()
  
  for (i in (columnSize+1) : dim(blockData)[1]) {
    #for (i in (51) : 100) {
    inputValues = data.frame(blockData[(i-columnSize):(i-1),"value"])
    outputValue = blockData[i,"value"]
    
    mlpData = rbind(mlpData, cbind(t(inputValues), outputValue, i))
  }
  
  return (mlpData)
}