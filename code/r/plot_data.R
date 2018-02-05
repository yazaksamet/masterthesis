setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
library("tseries")
library("ggplot2")

completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")
dayPeriod = 24

dataCount = ceiling(dim(completeData)[1]/dayPeriod)

groupData = data.frame(matrix(0, nrow = dataCount, ncol = 1))
colnames(groupData) <- c("index")
groupData$index = as.numeric(rownames(groupData))
groupData$group_start = (groupData$index - 1) * dayPeriod + 1
groupData$group_end = (groupData$group_start + dayPeriod - 1)
groupData$sum = 0

for(i in 1:dataCount) {
  groupData[i,"sum"] = sum(completeData[completeData$timestamp >= groupData[i,"group_start"] & completeData$timestamp <= groupData[i,"group_end"], "value"]);
}

explanation = paste0("Day:", dayPeriod/24)

ggplot() +
  geom_line(data = groupData, aes(x = index, y = sum, colour = explanation)) +
  ylab('Count')
