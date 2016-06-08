
#' Test SVM with radial kernel
#'
#' This functions look for best \emph{nu} and \emph{gamma} parameters for SVM algorithm with radial kernel.
#' It performs grid search. Checked values are ranging:
#' \itemize{
#'  \item 2^-16 to 2^-3, for gamma
#'  \item 2^-10 to 2^-1, for nu
#' }
#' To reduce time of execution only ramdom part of data is used.
#' Created model is evaluated by percentage of instances in which at least one of the attributes exceeded 'upper inner fence'.
#' This value is used because of unlabeled data used for training.
#' @param tablename table name with source data
#' @param sampe part of data used for creating model
#' @return Data frame with calculated values
#'
#' @export
testSvmRadial <- function(tablename, sample=0.1) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  boundVector <- getOutlierBound(data$instances)
  result <- gridSearchRadial(data$instances, boundVector, sample)
  result
}

#' Test SVM with linear kernel
#'
#' This functions look for best \emph{nu} parameter for SVM algorithm with linear kernel.
#' It performs grid search. Checked value of \emph{nu} is ranging: 2^-10 to 2^-1
#' To reduce time of execution only random part of data is used.
#' Created model is evaluated by percentage of instances in which at least one of the attributes exceeded 'upper inner fence'.
#' This value is used because of unlabeled data used for training.
#' Result of this function is plot of calculated values
#' @param tablename table name with source data
#' @param sampe part of data used for creating model
#' @return None

#' @export
testSvmLinear <- function(tablename, sample=0.1) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  boundVector <- getOutlierBound(data$instances)
  gridSearchLinear(data$instances, boundVector, sample)
}

#' Test K-medoids
#'
#' This functions look for best number of clusters for k-medoids .
#' It crates model for k ranging from 2 to 15 which is evaluated by silhouette calculated in \emph{cluster} package.
#' To reduce time of execution \link[cluster]{clara} algorithm is used.
#' Result of this function is plot of silhouette for each model.
#' @param tablename table name with source data
#' @param metric 'euclidean' or 'manhattan'
#' @param klimit upper bound of paramter k+
#' @return None
#'@export
testKmedoids <- function(tablename, metric, klimit=15) {
  rawData <- loadData(tablename)
  data <- prepareData(rawData)
  plotSilWidth(data$instances, metric, klimit)
}
