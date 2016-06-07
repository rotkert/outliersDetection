library(RMySQL)
library(properties)

#' Load data from database function
#'
#' This function allows to read data from database
#' @param tablename Name of the table in database
#' @keywords database
#' @return data frame object
#' @export
loadData <- function (tablename) {
  props <- read.properties(file = 'mow.properties', encoding='UTF-8')
  query <- paste('select * from', tablename, 'order by timestamp', sep=' ')
  mydb <-  dbConnect(MySQL(), user=props$user, password=props$password, dbname=props$dbname, host=props$host)
  rs <-  dbSendQuery(mydb, query)
  data <-  fetch(rs,n=-1)
  dbDisconnect(mydb)
  data
}

#' @export
loadAttributesDict <- function() {
  props <- read.properties(file = 'mow.properties', encoding='UTF-8')
  query <- "select concat('val', id), name from attribute_dict order by id"
  mydb <-  dbConnect(MySQL(), user=props$user, password=props$password, dbname=props$dbname, host=props$host)
  rs <-  dbSendQuery(mydb, query)
  attributeDict <-  fetch(rs,n=-1)
  dbDisconnect(mydb)
  attributeDict
}
