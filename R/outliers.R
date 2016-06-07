

#' @export
svmOutliers <- function(tablename, kernel, nu, gamma) {
  outliersFunction <- function(instances) svmOutliersIndexes(instances, kernel, nu, gamma)
  outliers <- outliers(tablename,outliersFunction)
  outliers
}



#' @export
kmedoidsOutliers <- function(tablename, k, metric, silLimit) {
  outliersFunction <- function(instances) kmedoidsOutliersIndexes(instances, k, metric, silLimit)
  outliers <- outliers(tablename,outliersFunction)
  outliers
}

#' @export
outliers <- function(tablename, detectorFunction) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  result <- execWithTimeMeasusre(function() detectorFunction(data$instances))

  outliers <-list()
  outliers$tablename <- tablename
  outliers$numAttributes <- ncol(data$instances)
  outliers$numInstances <- nrow(data$instances)
  outliers$instances <- data$instances[result$outliersIndexes, ]
  outliers$timestamps <- data$timestamps[result$outliersIndexes]
  outliers$time <- result$time
  outliers$params <- result$params
  outliers$boundVector <- getOutlierBound(data$instances)
  outliers$perColumn <- countColumnOutliers(outliers$instances, outliers$boundVector)
  outliers$perInstance <- countRowOutliers(outliers$instances, outliers$boundVector)
  outliers
}
