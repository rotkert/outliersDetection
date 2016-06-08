library(RMySQL)
library(properties)

loadData <- function (tablename) {
  props <- read.properties(file = 'mow.properties', encoding='UTF-8')
  query <- paste('select * from', tablename, 'order by timestamp', sep=' ')
  mydb <-  dbConnect(MySQL(), user=props$user, password=props$password, dbname=props$dbname, host=props$host)
  rs <-  dbSendQuery(mydb, query)
  data <-  fetch(rs,n=-1)
  dbDisconnect(mydb)
  data
}

loadAttributesDict <- function() {
  props <- read.properties(file = 'mow.properties', encoding='UTF-8')
  query <- "select concat('val_', id), name from attribute_dict order by id"
  mydb <-  dbConnect(MySQL(), user=props$user, password=props$password, dbname=props$dbname, host=props$host)
  rs <-  dbSendQuery(mydb, query)
  attributeDict <-  fetch(rs,n=-1)
  dbDisconnect(mydb)
  attributeDict
}
