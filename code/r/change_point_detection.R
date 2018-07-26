#install.packages("Rcpp")
#install.packages("ecp")
#install.packages("devtools")
#devtools::install_github("twitter/BreakoutDetection")

library(Rcpp)
library(ecp)
library(BreakoutDetection)

setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
completeData = read.csv(file="..\\..\\data\\workspace\\real_full_standard.csv", header=TRUE, sep=",")
completeData$timestampX = strptime(c("1.1.2000 00:00"), format = "%d.%m.%Y %H:%M", tz = "CET") + completeData$timestamp * 60 * 60

seasonalityResults = data.frame()

blockData = completeData[completeData$file_number == 48,]
stlRes = stl(ts(blockData$value, frequency = 24), s.window = "periodic", robust = TRUE)

blockData$nonseasonal = blockData$value - stlRes$time.series[,1]

testData = data.frame(blockData$timestampX, blockData$nonseasonal)
colnames(testData) <- c("timestamp", "count")

res = breakout(testData, plot=TRUE)
res$plot

