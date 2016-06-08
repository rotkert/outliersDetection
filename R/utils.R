
timestampToDate <- function(timestamp) {
  date <- as.POSIXct(timestamp, origin="1970-01-01")
  date
}

execWithTimeMeasusre <- function(functionToExec) {
  result <- list()

  start.time <- Sys.time()
  value <- functionToExec()
  result$outliersIndexes <- value$outliersIndexes
  result$params <- value$params
  end.time <- Sys.time()

  result$time <- end.time - start.time
  result$callDetails <- args(functionToExec)
  result
}
