#' @export
prepareData <- function(rawData) {
  data <- list()
  data$instances <- rawData[, -1]
  data$timestamp <- rawData[, 1]
  return(data)
}
