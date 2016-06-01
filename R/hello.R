#' @export
mow_start <- function(tablename, klimit) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)

  timestampOutliers <- data$timestamps[svmOutliers(data$instances)]
  timestampOutliers
}

#'@export
testKmeans <- function(tablename, klimit) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  plotSilWidth(data$instances, klimit)
}

#' @export
kmeansOutliers <- function(tablename, k, silLimit) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  outliersIndexes <- kmeansOutliersIndexes(data$instances, k, silLimit)
  timestampOutliers <- data$timestamps[outliersIndexes]
  timestampOutliers
}

#' @export
statsOutliers <- function(tablename, iqrCoefficient) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  outliersIndexes <- statsOutliersIndexes(data$instances, iqrCoefficient)
  timestampOutliers <- data$timestamps[outliersIndexes]
  timestampOutliers
}

#' @export
outliers <- function(tablename, detectorFunction) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  outliersIndexes <- detectorFunction(data$instances)
  timestampOutliers <- data$timestamps[outliersIndexes]
  timestampOutliers
}
