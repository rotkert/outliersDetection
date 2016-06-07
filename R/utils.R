
#' @export
timestampToDate <- function(timestamp) {
  as.POSIXct(timestamp, origin="1970-01-01")
}

execWithTimeMeasusre <- function(functionToExec) {
  result <- list()

  start.time <- Sys.time()
  result$value <- functionToExec()
  end.time <- Sys.time()

  result$time <- end.time - start.time
  result
}
