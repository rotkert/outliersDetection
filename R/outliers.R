#' @export
svmOutliers <- function(tablename) {
  outliersFunction <- function(instances) svmOutliersIndexes(instances)
  timestampOutliers <- outliers(tablename,outliersFunction)
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
  outliersFunction <- function(instances) kmeansOutliersIndexes(instances, k, silLimit)
  timestampOutliers <- outliers(tablename,outliersFunction)
  timestampOutliers
}

#' @export
outliers <- function(tablename, detectorFunction) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  result <- execWithTimeMeasusre(function() detectorFunction(data$instances))

  outliers <-list()
  outliers$tablename <- tablename
  outliers$instances <- data$instances[result$value, ]
  outliers$timestamps <- data$timestamps[result$value]
  outliers$time <- result$time
  outliers$boundVector <- getOutlierBound(data$instances)
  outliers$perAttribute <- countColumnOutliers(outliers$instances, outliers$boundVector)
  outliers$perInstance <- countRowOutliers(outliers$instances, outliers$boundVector)
  outliers
}
