
#' @export
timestampToDate <- function(timestamp) {
  as.POSIXct(timestamp, origin="1970-01-01")
}
