library(clusterSim)

prepareData <- function(rawData) {
  data <- list()
  unnormalizedData <- rawData[, -1]
  zerSDColumns <- getZeroSD(unnormalizedData)
  if (length(zerSDColumns) > 0)
    unnormalizedData <- subset(unnormalizedData, select=-c(zerSDColumns))
  data$instances <- data.Normalization(unnormalizedData, type='n1', normalization = 'column')
  data$timestamps <- rawData[, 1]
  data
}
