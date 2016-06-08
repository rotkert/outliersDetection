library(xlsx)
library(plotly)

#' Analyze outliers
#'
#' This functions allows to prepare summary of outliers detection algorithms.
#' It creates directory containing .xlsx file with details of algorithm execution and output.
#' It also creates two charts:
#' \itemize{
#'  \item presenting, for each attribute, number of instances in which this attribute exceeded vaue \emph{quantile3 + 1.5 * IQR}
#'  \item presenting, in time period, nubmer of attributes which exceeded value  of 'upper inner fence' \emph{quantile3 + 1.5 * IQR}
#' }
#' @param o outliers object returned by \link[mow]{svmOutliers} or \link[mow]{kmedoidsOutliers}
#' @param dirName directory in which data should be saved
#' @return None
#'
#' @export
analyze <- function(o, dirName) {
  dirPath = paste('results\\', dirName, sep='')
  filePath = paste(dirPath, '\\results.xlsx', sep='')
  dir.create(dirPath)

  instanceOutliers <- o$perInstance
  instancesWithOutliers <- countInsstanceOutliers(instanceOutliers)
  values <- c(o$tablename, o$numAttributes, o$numInstances, nrow(o$instances), o$time, instancesWithOutliers, o$params)
  names <- c('table name', 'number of attributes','number of all instances','number of outliers', 'execution time', 'Percent of instances with stat outliers', 'params')
  generalInfo <- data.frame(names, values)
  write.xlsx(generalInfo, filePath, 'generalInfo', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.xlsx(instanceOutliers, filePath, 'instanceOutliers', append = TRUE)

  timestamps <- data.frame(o$timestamps)
  saveTimestampChart(o$timestamps, instanceOutliers, dirPath)
  write.xlsx(timestamps, filePath, 'timestamps', row.names = FALSE, append = TRUE)

  attributesOutliers <- getAttributesOutliers(o$instances, o$perColumn)
  saveAttributeOutliersChart(attributesOutliers, dirPath)
  write.xlsx(attributesOutliers, filePath, 'attributesOutliers', row.names = FALSE, append = TRUE)
}

getAttributesOutliers <- function(instances, columnOutliers) {
  attributeDict <- loadAttributesDict()
  colNames <- colnames(instances)
  attributeNames <- sapply(1:length(colNames), function(i) attributeDict[attributeDict[,1] == colNames[i], 2])
  attributeOutliersDf <- data.frame(attributeNames, columnOutliers)
  attributeOutliersDf
}

countInsstanceOutliers <- function(io) {
  withOutliers <- length(io[io > 0])
  all <- length(io)
  percentage <- withOutliers/all * 100
  percentage
}

saveAttributeOutliersChart <- function(ao, dirPath) {
  filePath = paste(dirPath, '\\attributeOutliers.png', sep='')
  p <- plot_ly(x = ao$attributeNames, y = ao$columnOutliers, type = 'bar')
  plotly_IMAGE(p, format = "png", out_file = filePath)
}

saveTimestampChart <- function(timestamps, instancesOutliers, dirPath) {
  filePath = paste(dirPath, '\\timestamps.png', sep='')
  dates <- timestampToDate(timestamps)
  pl <- plot_ly(x=dates, y=instancesOutliers, mode="markers")
  plotly_IMAGE(pl, format = "png", out_file = filePath)
}
