library(MASS)
library(e1071)
library(randomForest)
library(caret)

ldaWrapper <- function(trainIndexes,
                       testIndexes,
                       dataset,
                       selectedFeatures) {
  
  p <- partition(scale(dataset$X),
                 selectedFeatures,
                 dataset$Y,
                 trainIndexes,
                 testIndexes)
  
#   if (length(selectedFeatures) > 1) {
#     
#     dataPreProcess <- preProcess(p$trainData, method = c("YeoJohnson", "center", "scale"))
#     p$trainData <- predict(dataPreProcess, p$trainData)
#     p$testData <- predict(dataPreProcess, p$testData)
#     
#     nzvTrain <- nearZeroVar(p$trainData, freqCut = 85/15)
#     nzvTest <- nearZeroVar(p$testData, freqCut = 85/15)
#     nzv <- union(nzvTrain, nzvTest)
#     if (length(nzv) > 0) {
#       p$trainData <- p$trainData[, -nzv]
#       p$testData <- p$testData[, -nzv]
#     }
#     
#     trainCor <- cor(p$trainData)
#     hcTrain <- findCorrelation(trainCor)
#     testCor <- cor(p$testData)
#     hcTest <- findCorrelation(testCor)
#     highCor <- union(hcTrain, hcTest)
#     if (length(highCor) > 0) {
#       p$trainData <- p$trainData[, -highCor]
#       p$testData <- p$testData[, -highCor]
#     }
#   }
  
  model <- lda(p$trainData,
               p$trainLabels,
               tol = 1.0e-6)
  
  predictions <- predict(model, p$testData)
  
  ldaPredictionInfo <- list(predictedClasses = predictions$class,
                            probabilities = predictions$posterior)
  
  return(ldaPredictionInfo)
}

svmWrapper <- function(trainIndexes,
                       testIndexes,
                       dataset,
                       selectedFeatures) {
  
  p <- partition(scale(dataset$X),
                 selectedFeatures,
                 dataset$Y,
                 trainIndexes,
                 testIndexes)
  
  model <- svm(p$trainData,
               p$trainLabels,
               probability = TRUE)
  
  predictedClasses <- predict(model, p$testData, probability = TRUE)
  probabilities <- attr(predictedClasses, "probabilities")
  
  linearSvmPredictionInfo <- list(predictedClasses = predictedClasses,
                                  probabilities = probabilities)
  
  return(linearSvmPredictionInfo)
}

randomForestWrapper <- function(trainIndexes,
                                testIndexes,
                                dataset,
                                selectedFeatures) {
  
  p <- partition(scale(dataset$X),
                 selectedFeatures,
                 dataset$Y,
                 trainIndexes,
                 testIndexes)
  
  model <- randomForest(p$trainData,
                        p$trainLabels)
  
  predictedClasses <- predict(model, p$testData)
  probabilities <- predict(model, p$testData, type = "prob")
  
  randomForestPredictionInfo <- list(predictedClasses = predictedClasses,
                                     probabilities = probabilities)
  
  return(randomForestPredictionInfo)
}

# Utility function for helping in data partitioning
partition <- function(data,
                      selectedFeatures,
                      labels,
                      trainIndexes,
                      testIndexes) {
  
  list(trainData = as.matrix(data[trainIndexes, selectedFeatures]),
       testData  = as.matrix(data[ testIndexes, selectedFeatures]),
       trainLabels = as.factor(labels[trainIndexes]),
       testLabels  = as.factor(labels[ testIndexes]))
}

