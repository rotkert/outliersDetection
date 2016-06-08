library(plyr)
library(reshape2)

countColumnOutliers <- function(outInstances, boundVector) {
  columnOutliers <- sapply(1:ncol(outInstances), function(i) columnOutliersInt(i, outInstances, boundVector))
  columnOutliers
}

columnOutliersInt <-function(i, outInstances, boundVector) {
  x <- outInstances[,i]
  length(x[x>boundVector[i]])
}

countRowOutliers <- function(outInstances, boundVector) {
  rowOutliers <- apply(outInstances, 1, function(x) rowOutliersInt(x, boundVector))
  rowOutliers
}

rowOutliersInt <- function(row, boundVector) {
  outliersAmount <- 0
  for(i in 1:length(row))
    if(row[i] > boundVector[i])
      outliersAmount <- outliersAmount + 1

  outliersAmount
}

getOutlierBound <- function(instances, iqrCoefficient = 1.5) {
  meltedInsatnces <- melt(instances)
  statsValues <- ddply(meltedInsatnces, c("variable"), summarise, quantile3=quantile(value)[3], IQR = IQR(value))
  boundVector <- statsValues$quantile3 + iqrCoefficient * statsValues$IQR
  boundVector
}

getZeroSD <- function(instances) {
  meltedInsatnces <- melt(instances)
  instancesSD <- ddply(meltedInsatnces, c("variable"), summarise, sd=sd(value))
  zeroSD <- instancesSD[which(instancesSD$sd == 0), ]
  zeroSD$variable
}
