setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")

#install.packages("gridExtra")

library(forecast)
library(stats)
library(TSA)
library(FBN)
library(robustbase)
library(ggplot2)
library(gtable)
library(gridExtra)

source("seasonality.r")

completeData = read.csv(file="..\\..\\data\\workspace\\synthetic_full_standard.csv", header=TRUE, sep=",")
seasonalityResults = data.frame()

blockData = completeData[completeData$file_number == 9,]

#stl decomposition
stlRes = stl(ts(blockData$value, frequency = 189), s.window = "periodic", robust = TRUE)
plot(stlRes)

#decompose decomposition
decomposeRes = decompose(ts(blockData$value, frequency = 135), "additive")
plot(decomposeRes)

#manual decomposition
#blockData$trend = ma(blockData$value, order = 131, centre = T)
blockData$trend = runmed(blockData$value, 135)
detrend = blockData$value - blockData$trend
m = t(matrix(data = detrend, nrow = 135))
seasonal = colMedians(m, na.rm = T)
blockData$random = detrend - seasonal
blockData$detrend = detrend

seasonalBlock = rep(seasonal, floor(dim(blockData)[1] / length(seasonal)))
remainingSeasonal = (dim(blockData)[1] - length(seasonalBlock))

if (remainingSeasonal > 0) {                     
  seasonalBlock = c(seasonalBlock, head(seasonal, remainingSeasonal))
}
blockData$seasonal = seasonalBlock

plot(seasonal)

p2 = ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = value, colour = "Original"))
p3 = ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = seasonal, colour = "seasonal"))
p4 = ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = trend, colour = "trend"))
p5 = ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = random, colour = "random"))

p6 = ggplot() +
  geom_line(data = blockData, aes(x = timestamp, y = detrend, colour = "detrend"))


grid.arrange(p2, p3, p4, p5, p6, nrow=5, ncol=1)

for (i in 45:45) {
  blockData = completeData[completeData$file_number == i,]
  
  seasonalityResult = getSeasonality(blockData$value)
  
  seasonalityResults = rbind(seasonalityResults, c(i, seasonalityResult[["hasSeasonality"]], 
                                                   seasonalityResult[["periodLength"]]))
}