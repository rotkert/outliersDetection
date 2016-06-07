library(e1071)

#' @export
svmOutliersIndexes <- function(instances) {
  model <- svm(instances, type="one-classification", kernel="linear", nu=0.01)
  outliersIndexes <- which(fitted(model) == FALSE)
  outliersIndexes
}
