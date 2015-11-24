
library(ggplot2)
library(rgl)
library(RColorBrewer)

syntheticDatasets <- function() {
  
  ################################ Synthetic Dataset 1 ####################################
  
  d1_feature1 <- c(rnorm(500, mean = 2, sd = 1),
                   rnorm(500, mean = 12, sd = 1))
  
  d1_noise <- sapply(1:99, function (i) runif(1000))
  colnames(d1_noise) <- paste("feature", 2:100, sep = "")
  
  Y <- c(rep(0, 500), rep(1, 500))
  Y <- factor(Y, labels = c("C1", "C2"))
  
  d1_X <- data.frame(feature1 = d1_feature1, d1_noise)
  
  noisyDataset1_ <- list(
    name = "Noisy_Dataset_1",
    X = d1_X,
    Y = Y,
    data = function () {
      data.frame(d1_X, Y = Y)
    }
  )
  
  ################################ Synthetic Dataset 2 ####################################
  
  d2_feature1 <- c(rnorm(500, mean = 2, sd = 1),
                   rnorm(500, mean = 12, sd = 1))
  d2_feature2 <- c(rnorm(500, mean = 2, sd = 1),
                   rnorm(500, mean = 12, sd = 1))
  
  d2_noise <- sapply(1:98, function (i) runif(1000))
  colnames(d2_noise) <- paste("feature", 3:100, sep = "")
  
  d2_X <- data.frame(feature1 = d2_feature1,
                     feature2 = d2_feature2,
                     d2_noise)
  
  noisyDataset2_ <- list(
    name = "Noisy_Dataset_2",
    X = d2_X,
    Y = Y,
    data = function () {
      data.frame(d2_X, Y = Y)
    }
  )
  
  ################################ Synthetic Dataset 3 ####################################
  
  d3_feature1 <- c(rnorm(500, mean = 2, sd = 1),
                   rnorm(500, mean = 12, sd = 1))
  d3_feature2 <- c(rnorm(500, mean = 2, sd = 1),
                   rnorm(500, mean = 12, sd = 1))
  d3_feature3 <- c(rnorm(500, mean = 2, sd = 1),
                   rnorm(500, mean = 12, sd = 1))
  
  d3_noise <- sapply(1:97, function (i) runif(1000))
  colnames(d3_noise) <- paste("feature", 4:100, sep = "")
  
  d3_X <- data.frame(feature1 = d3_feature1,
                     feature2 = d3_feature2,
                     feature3 = d3_feature3,
                     d3_noise)
  
  noisyDataset3_ <- list(
    name = "Noisy_Dataset_3",
    X = d3_X,
    Y = Y,
    data = function () {
      data.frame(d3_X, Y = Y)
    }
  )
  
  ################################ Synthetic Dataset 4 ####################################
  
  d4_feature <- c(rnorm(500, mean = 2, sd = 1),
                  rnorm(500, mean = 12, sd = 1))
  
  multicollinearData1 <- replicate(100, d4_feature)
  colnames(multicollinearData1) <- paste("feature", 1:100, sep = "")
  
  d4_X <- data.frame(multicollinearData1)
  
  multicollinearDataset1_ <- list(
    name = "Multicollinear_Dataset_1",
    X = d4_X,
    Y = Y,
    data = function () {
      data.frame(d4_X, Y = Y)
    }
  )
  
  ################################ Synthetic Dataset 5 ####################################
  
  d5_feature <- c(rnorm(500, mean = 2, sd = 1),
                  rnorm(500, mean = 12, sd = 1))
  
  multicollinearData2 <- replicate(100, d5_feature)
  colnames(multicollinearData2) <- paste("feature", 1:100, sep = "")
  
  d5_X <- data.frame(multicollinearData2)
  
  multicollinearDataset2_ <- list(
    name = "Multicollinear_Dataset_2",
    X = d5_X,
    Y = Y,
    data = function () {
      data.frame(d5_X, Y = Y)
    }
  )
  
  
  ################################ Synthetic Dataset 6 ####################################
  
  d6_feature <- c(rnorm(500, mean = 2, sd = 1),
                  rnorm(500, mean = 12, sd = 1))
  
  zeroVarianceData <- replicate(99, rep(0, 1000))
  zeroVarianceData <- sapply(
    1:99, function(n) zeroVarianceData[,n] <<- zeroVarianceData[,n] + n)
  
  colnames(zeroVarianceData) <- paste("feature", 2:100, sep = "")
  
  d6_X <- data.frame(feature1 = d6_feature,
                     zeroVarianceData)
  
  zeroVarianceDataset1_ <- list(
    name = "ZeroVariance_Dataset_1",
    X = d6_X,
    Y = Y,
    data = function () {
      data.frame(d6_X, Y = Y)
    }
  )
  
  syntheticDatasets <- list(
    noisyDataset1 = noisyDataset1_,
    noisyDataset2 = noisyDataset2_,
    noisyDataset3 = noisyDataset3_,
    multicollinearDataset1 = multicollinearDataset1_,
    multicollinearDataset2 = multicollinearDataset2_)
}

plotSyntheticDatasets <- function(syntheticDatasets) {
  
  p1 <- ggplot(syntheticDatasets[[1]]$data(), aes(x = feature1))
  print(p1 + geom_density())
  
  p2 <- ggplot(syntheticDatasets[[2]]$data(), aes(feature1, feature2))
  print(p2 + geom_point(aes(colour = Y)))
  
  plot3d(syntheticDatasets[[3]]$X$feature1,
         syntheticDatasets[[3]]$X$feature2,
         syntheticDatasets[[3]]$X$feature3,
         xlab = "feature1",
         ylab = "feature2",
         zlab = "feature3",
         size = 4,
         col = brewer.pal(3, "Dark2")[unclass(syntheticDatasets[[3]]$Y)])
  
}

plotSyntheticDatasets(syntheticDatasets())
