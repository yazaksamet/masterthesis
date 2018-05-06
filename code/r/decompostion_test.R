set.seed(4)
data <- read.csv("webTraffic.csv", sep = ",", header = T)
days = as.numeric(data$Visite)
for (i in 1:45 ) {
  pos = floor(runif(1, 1, 50))
  days[i*15+pos] = days[i*15+pos]^1.2
}
days[510+pos] = 0

library(forecast)
library(stats)

trend = runmed(days, 7)
plot(as.ts(trend))
detrend = days / as.vector(trend)
m = t(matrix(data = detrend, nrow = 7))
seasonal = colMeans(m, na.rm = T)
random = days / (trend * seasonal)
rm_random = runmed(random[!is.na(random)], 3)

min = mean(rm_random, na.rm = T) - 4*sd(rm_random, na.rm = T)
max = mean(rm_random, na.rm = T) + 4*sd(rm_random, na.rm = T)
plot(as.ts(random))
abline(h=max, col="#e15f3f", lwd=2)
abline(h=min, col="#e15f3f", lwd=2)

position = data.frame(id=seq(1, length(random)), value=random)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value)]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
points(x = anomaly$id, y =anomaly$value, col="#e15f3f")

plot(as.ts(days))
real = data.frame(id=seq(1, length(days)), value=days)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")