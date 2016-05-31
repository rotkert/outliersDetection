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
  query <- paste('select * from', tablename, sep=' ')
  mydb <-  dbConnect(MySQL(), user=props$user, password=props$password, dbname=props$dbname, host=props$host)
  rs <-  dbSendQuery(mydb, query)
  data <-  fetch(rs,n=-1)
  return(data)
}
