
library(MASS)
library(e1071)
library(randomForest)

source(file.path("utils", "utils.R"))

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

linearSVMWrapper <- function(trainIndexes,
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
               kernel = "linear",
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

