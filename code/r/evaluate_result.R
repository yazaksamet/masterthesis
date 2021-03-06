setwd("C:/Users/sametyazak/Desktop/ynwa/bau/2017 - Thesis/code/r/")
fullResultSet = read.csv(file="tsd_result.csv", header=TRUE, sep=",")
fullResultSet = cbind(fullResultSet, 
      recall = (fullResultSet$tp / (fullResultSet$tp+fullResultSet$fn)),
      precision = fullResultSet$tp / (fullResultSet$tp+fullResultSet$fp)
)

fullResultSet = cbind(fullResultSet, f1_score = 2 * (fullResultSet$precision*fullResultSet$recall) / (fullResultSet$precision+fullResultSet$recall))
write.csv2(fullResultSet, "result_view.csv")