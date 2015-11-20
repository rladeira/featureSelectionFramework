library(MASS)
library(e1071)
library(randomForest)

ldaWrapper <- function(trainIndexes,
                       testIndexes,
                       dataset,
                       selectedFeatures) {
  
  p <- partition(scale(dataset$X),
                 selectedFeatures,
                 dataset$Y,
                 trainIndexes,
                 testIndexes)
  
  model <- lda(p$trainData,
               p$trainLabels)
  
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

