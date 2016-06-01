#' @export
prepareData <- function(rawData) {
  data <- list()
  data$instances <- rawData[, -1]
  data$timestamps <- rawData[, 1]
  data
}
