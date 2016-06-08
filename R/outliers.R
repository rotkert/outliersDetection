
#' Support Vector Machine's outliers
#'
#' This function finds outliers in data from given table in db. The first column of data is supposed to be timestamp.
#' Before performing algorithm, data is normalized and attributes with zero standard deviation are removed.
#' To detect anomalies function uses svm algorithm from \link[e1071]{svm}
#' @param tablename Name of the table in database from which data is loaded
#' @param kerner Type of kernel used by svm algorithm (linear or radial)
#' @param nu Parameter nu of svm algorith
#' @param gamma Parameter gamma of svm algorithm (used only in radial kernel)
#' @return Outliers object; a list consisting of:
#'  \item{tablename}{Source tablename for data instances}
#'  \item{numAttributes}{Number of attributes in given data}
#'  \item{numInstances}{Number of instances in given data}
#'  \item{instances}{Instances detect as outliers}
#'  \item{timestamps}{Timestamps for which outliers where detected}
#'  \item{time}{Time of algorithm execution}
#'  \item{params}{Prameters of algorithm which were used}
#'  \item{boundVector}{Vector containing for each column value of 'upper inner fence': \emph{quantile3 + 1.5 * IQR}}
#'  \item{perColumn}{Vector containing number of outliers per each column of data}
#'  \item{perInstance}{Vector containing number of outliers per each instance of data}
#'
#' @export
svmOutliers <- function(tablename, kernel, nu, gamma) {
  outliersFunction <- function(instances) svmOutliersIndexes(instances, kernel, nu, gamma)
  outliers <- outliers(tablename,outliersFunction)
  outliers
}


#' K-medoids outliers
#'
#' This function finds outliers in data from given table in db. The first column of data is supposed to be timestamp.
#' Before performing algorithm, data is normalized and attributes with zero standard deviation are removed.
#' To detect anomalies function uses k-medoids algorithm from \link[cluster]{pam}
#' @param tablename Name of the table in database from which data is loaded
#' @param k number of clusters
#' @param metric 'euclidean' or 'manhattan'
#' @param silLImit Silhouette limit over which instances are considered outliers
#' @return Outliers object; a list consisting of:
#'  \item{tablename}{Source tablename for data instances}
#'  \item{numAttributes}{Number of attributes in given data}
#'  \item{numInstances}{Number of instances in given data}
#'  \item{instances}{Instances detect as outliers}
#'  \item{timestamps}{Timestamps for which outliers where detected}
#'  \item{time}{Time of algorithm execution}
#'  \item{params}{Prameters of algorithm which were used}
#'  \item{boundVector}{Vector containing for each column value of 'upper inner fence': \emph{quantile3 + 1.5 * IQR}}
#'  \item{perColumn}{Vector containing number of outliers per each column of data}
#'  \item{perInstance}{Vector containing number of outliers per each instance of data}
#'
#' @export
kmedoidsOutliers <- function(tablename, k, metric, silLimit) {
  outliersFunction <- function(instances) kmedoidsOutliersIndexes(instances, k, metric, silLimit)
  outliers <- outliers(tablename,outliersFunction)
  outliers
}

outliers <- function(tablename, detectorFunction) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  result <- execWithTimeMeasusre(function() detectorFunction(data$instances))

  outliers <-list()
  outliers$tablename <- tablename
  outliers$numAttributes <- ncol(data$instances)
  outliers$numInstances <- nrow(data$instances)
  outliers$instances <- data$instances[result$outliersIndexes, ]
  outliers$timestamps <- data$timestamps[result$outliersIndexes]
  outliers$time <- result$time
  outliers$params <- result$params
  outliers$boundVector <- getOutlierBound(data$instances)
  outliers$perColumn <- countColumnOutliers(outliers$instances, outliers$boundVector)
  outliers$perInstance <- countRowOutliers(outliers$instances, outliers$boundVector)
  outliers
}
