#' @export
testSvmRadial <- function(tablename, sample=0.1) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  boundVector <- getOutlierBound(data$instances)
  result <- gridSearchRadial(data$instances, boundVector, sample)
  result
}

#' @export
testSvmLinear <- function(tablename, sample=0.1) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  boundVector <- getOutlierBound(data$instances)
  gridSearchLinear(data$instances, boundVector, sample)
}

#'@export
testKmedoids <- function(tablename, metric, klimit=15) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  plotSilWidth(data$instances, metric, klimit)
}
