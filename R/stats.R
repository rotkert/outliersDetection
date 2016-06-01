library(plyr)
library(reshape2)


statsOutliersIndexes <- function(instances, iqrCoefficient) {
  meltedInsatnces <- melt(instances)
  statsValues <- ddply(meltedInsatnces, c("variable"), summarise, quantile3=quantile(value)[3], IQR = IQR(value))
  iqrVector <- statsValues$quantile3 + iqrCoefficient * statsValues$IQR
  outliersIndexes <- which(apply(instances, 1, function(x) all(x > iqrVector)) == TRUE)
  outliersIndexes
}
