#' @export
mow_start <- function() {
  rawData <- loadData('bolek order by timestamp limit 10')
  data <- prepareData(rawData)

}
