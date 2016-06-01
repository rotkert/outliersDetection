library(e1071)

#' @export
svmOutliers <- function(instances) {
  print("svm")
  model <- svm(instances, type="one-classification", kernel="linear", nu=0.01)
  outliersIndexes <- which(fitted(model) == FALSE)
  outliersIndexes
}
