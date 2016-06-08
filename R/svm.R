library(e1071)

svmOutliersIndexes <- function(instances, kernel, nu, gamma) {
  result <- list()
  result$params <- paste('kernel: ', kernel, 'nu: ', nu, 'gamma: ', gamma, sep=' ')
  model <- svm(instances, type="one-classification", kernel=kernel, nu=nu, gamma=gamma)
  result$outliersIndexes <- which(fitted(model) == FALSE)
  result
}

gridSearchRadial <- function(instances, boundVector, sample) {
  sampleSize <- nrow(instances) * sample
  sampleInstances <- instances[sample(nrow(instances), sampleSize), ]
  results <- data.frame()
  for(i in 1:10) {
    ipow <- i * 2 - 17
    gamma <- 2 ^ ipow
    for(j in 1:10) {
      jpow <- j - 11
      nu <- 2 ^ jpow
      result <- svmOutliersIndexes(sampleInstances, 'radial', nu, gamma)
      outliersIndexes <- result$outliersIndexes
      rowOutliers <- countRowOutliers(sampleInstances[outliersIndexes, ], boundVector)
      realOutliersPrc <- countInsstanceOutliers(rowOutliers)
      results[i,j] <- realOutliersPrc
     }
  }

  results
}

gridSearchLinear <-function(instances, boundVector, sample) {
  gamma <- 1
  sampleSize <- nrow(instances) * sample
  sampleInstances <- instances[sample(nrow(instances), sampleSize), ]
  realOutliersPrc <- array(1:10)
  for (i in 1:10) {
    ipow <- i -11
    nu <- 2 ^ ipow
    result <- svmOutliersIndexes(sampleInstances, 'linear', nu, gamma)
    outliersIndexes <- result$outliersIndexes
    rowOutliers <- countRowOutliers(sampleInstances[outliersIndexes, ], boundVector)
    realOutliersPrc[i] <- countInsstanceOutliers(rowOutliers)
  }

  plot(1:10, realOutliersPrc, type="b")
}
