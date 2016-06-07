library(xlsx)
library(plotly)

#' @export
analyze <- function(o, dirName) {
  dirPath = paste('results\\', dirName, sep='')
  filePath = paste(dirPath, '\\results.xlsx', sep='')
  dir.create(dirPath)

  instanceOutliers <- o$perInstance
  instancesWithOutlies <- countInsstanceOutliers(instanceOutliers)
  values <- c(o$tablename, o$time, instancesWithOutlies)
  names <- c('table name', 'execution time', 'Percent of instances with stat outliers')
  generalInfo <- data.frame(names, values)
  write.xlsx(generalInfo, filePath, 'generalInfo', row.names = FALSE, col.names = FALSE, append = TRUE)
  write.xlsx(instanceOutliers, filePath, 'instanceOutliers', append = TRUE)

  timestamps <- data.frame(o$timestamps)
  saveTimestampChart(o$timestamps, dirPath)
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

saveTimestampChart <- function(timestamps, dirPath) {
  filePath = paste(dirPath, '\\timestamps.png', sep='')
  dates <- timestampToDate(timestamps)
  oneVector <- c(1)
  dullVector <- rep(oneVector, length(dates))
  pl <- plot_ly(x=dates, y=dullVector, mode="markers")
  plotly_IMAGE(pl, format = "png", out_file = filePath)
}
