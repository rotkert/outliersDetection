library(cluster)

plotSilWidth <- function(instances, metric, klimit) {
  silWidth <- array(1:klimit)
  for (i in 2:klimit)
    silWidth[i] <- clara(instances, i, metric = metric)$silinfo$avg.width

  plot(1:klimit, silWidth, type="b", xlab="Number of Clusters", ylab="Avg sil width")
}

kmedoidsOutliersIndexes <- function(instances, k, metric, silLimit) {
  result <- list()
  result$params <- paste('k: ', k, 'silLimit: ', silLimit, sep='')
  model <- pam(instances, k, metric = metric)
  silInfo <- silhouette(model, full=TRUE)
  result$outliersIndexes <- which(silInfo[, 3] < silLimit)
  result
}
