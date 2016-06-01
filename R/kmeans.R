library(cluster)

#' @export
plotSilWidth <- function(instances, klimit) {
  silWidth <- array(1:klimit)
  for (i in 2:klimit)
    silWidth[i] <- clara(instances, i)$silinfo$avg.width

  plot(1:klimit, silWidth, type="b", xlab="Number of Clusters", ylab="Avg sil width")
}

#' @export
kmeansOutliersIndexes <- function(instances, k, silLimit) {
  print("kmeans")
  model <- clara(instances, k)
  silInfo <- silhouette(model, full=TRUE)
  outliersIndexes <- which(silInfo[, 3] < silLimit)
  outliersIndexes
}
